#include "conflag.h"
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>

#define NUM_ARGS(...) (sizeof((void*[]){NULL, ##__VA_ARGS__})/sizeof(void*)-1)
#define CHECK_MEM(...) check_mem(NUM_ARGS(__VA_ARGS__), ##__VA_ARGS__)
#define ALLOC(type) (type*) alloc(sizeof(type))
#define ASSERT_TYPE(o, t) if (o.type != t) \
  THROW(CONFLAG_TYPE_ERROR, "%s expected type %s, got type %s", \
      __func__, conflag_type_to_string(t), conflag_type_to_string(o.type))

#define PERTURB_BITS 5
#define HASH_NEXT(j, p) j = (5 * j) + 1 + p, p >>= PERTURB_BITS

#define THROW(err, m...) ((sprintf(error_msg, ##m)), throw(err, __LINE__))
#define THROW_OOM() THROW(CONFLAG_NO_MEMORY, "Out of memory!");
#define TRY { jmp_buf _jmpctx_backup__ = {*jmpctx}; \
    conflag_error_type _err__; if (!(_err__ = setjmp(jmpctx)))
#define CATCH(err) else if (_err__ == err && \
    reset_jmpbuf(jmpctx, _jmpctx_backup__))
#define CATCH_ALL else if (_err__ && reset_jmpbuf(jmpctx, _jmpctx_backup__))
#define END_TRY else { *jmpctx = *_jmpctx_backup__; throw(_err__, __LINE__); } \
    *jmpctx = *_jmpctx_backup__; }
#define EXCEPTION _err__

#define LOGIC_ERROR \
    (conflag_ref) {CONFLAG_ERROR, {.error = CONFLAG_LOGIC_ERROR}}

static __thread jmp_buf jmpctx;
static void* (*_calloc)(size_t, size_t) = calloc;
static void (*_free)(void*) = free;

void conflag_set_allocators(
    void * (*calloc)(size_t, size_t), void (*free)(void*)) {
  _calloc = calloc;
  _free = free;
}

//TODO: Utility to print ASTs

#define MAX_ERROR_MSG_SIZE 1024
static __thread char error_msg[MAX_ERROR_MSG_SIZE];

void throw(int exc, int lineno) {
  printf("Throwing %d from line %d: %s\n", exc, lineno, error_msg);
  longjmp(jmpctx, exc);
}

int reset_jmpbuf(jmp_buf buf, jmp_buf backup) {
  printf("CAUGHT!\n");
  *buf = *backup;
  return 1;
}


#define CONFLAG_TRUE (conflag_ref) {CONFLAG_BOOLEAN, {.boolean = 1}}
#define CONFLAG_FALSE (conflag_ref) {CONFLAG_BOOLEAN, {.boolean = 0}}
#define CONFLAG_NULL (conflag_ref) {CONFLAG_NULL_TYPE, {0}}


/**
 * If any of the passed in pointers are null, free all non-null pointers
 * passed and throw OOM.
 */
void check_mem(int num_args, ...) {
  va_list args;
  va_start(args, num_args);
  for (int i = 0; i < num_args; ++i) {
    if (!va_arg(args, void*)) {
      va_end(args);
      va_start(args, num_args);
      for (int j = 0; j < num_args; ++j) {
        void *ptr;
        if ((ptr = va_arg(args, void*))) {
          _free(ptr);
        }
      }
      va_end(args);
      printf("throwing\n");
      THROW_OOM();
    }
  }
  va_end(args);
}

void *alloc(int size) {
  void *ptr = _calloc(size, 1);
  CHECK_MEM(ptr);
  return ptr;
}


conflag_ref resolve(conflag_object *object, conflag_string *name);
conflag_ref execute_call(conflag_call *call);
conflag_ref execute_native_call(
    const conflag_native_function *func, conflag_call *call);
conflag_ref plus(const conflag_plus *plus);
conflag_ref value(conflag_ref ref);
int equals(conflag_ref a, conflag_ref b);
conflag_object *builtin_scope();

conflag_ref dict_get(conflag_dict *dict, conflag_string *key);
void dict_insert(conflag_dict *dict, conflag_string *key, conflag_ref value);
conflag_dict *dict_new(long size);
void dict_free(conflag_dict *dict);

long hash_string(char *str, int length) {
  long hash = *str << 7;
  while (--length > 0) {
    hash = (1000003 * hash) ^ *(str++);
  }
  return hash ^ length;
}

void _conflag_free(conflag_ref ref);


int get_long(conflag_object *cfg, char *query, long *value) {
  conflag_ref ref = get(cfg, query);
  if (ref.type != CONFLAG_NUMBER || ref.number->type != CONFLAG_NUMBER_INTEGER) {
    return 1;
  }
  *value = ref.number->l;
  return 0;
}

int get_double(conflag_object *cfg, char *query, double *value) {
  conflag_ref ref = get(cfg, query);
  if (ref.type != CONFLAG_NUMBER || ref.number->type != CONFLAG_NUMBER_DOUBLE) {
    return 1;
  }
  *value = ref.number->d;
  return 0;
}

int get_string(conflag_object *cfg, char *query, char **value) {
  conflag_ref ref = get(cfg, query);
  printf("Get returned type %d\n", ref.type);
  if (ref.type == CONFLAG_ERROR) {
    printf("Get returned error! %d\n", ref.error);
  }
  if (ref.type != CONFLAG_STRING) return 1;
  *value = ref.string->string;
  return 0;
}

int get_boolean(conflag_object *cfg, char *query, int *value) {
  conflag_ref ref = get(cfg, query);
  printf("Get returned type %d\n", ref.type);
  if (ref.type == CONFLAG_ERROR) {
    printf("Get returned error! %d\n", ref.error);
  }
  if (ref.type != CONFLAG_BOOLEAN) return 1;
  *value = ref.boolean;
  return 0;
}

conflag_ref get(conflag_object *cfg, char *query) {
  TRY {
    //TODO: Should be able to make a local conflag_string and pass ptr instead
    conflag_string *key = conflag_string_from_cstr(query, strlen(query));
    conflag_ref ret = value(dict_get(cfg->dict, key));
    _free(key->string);
    _free(key); //TODO: Maybe need special free funcs; also can leak on error
    return ret;
  } CATCH_ALL {
    printf("Caught exception %d\n", EXCEPTION);
    return (conflag_ref) {CONFLAG_ERROR, {.error = EXCEPTION}};
  } END_TRY;
  return LOGIC_ERROR;
}


void dict_insert(conflag_dict *dict, conflag_string *key, conflag_ref value) {
  for (long j = key->hash, p = key->hash; ; HASH_NEXT(j, p)) {
    conflag_dict_entry *entry = dict->table + (j & dict->mask);
    //TODO: Can just use null key instead of CONFLAG_DICT_EMPTY!!!
    if (entry->ref.type == CONFLAG_DICT_EMPTY) {
      *entry = (conflag_dict_entry) {key, value};
      dict->used++;
      break;
    } else if (key->length == entry->key->length &&
         !strncmp(key->string, entry->key->string, key->length)) {
      entry->ref = value;
      break;
    }
  }
}

conflag_ref dict_get(conflag_dict *dict, conflag_string *key) {
  for (long j = key->hash, p = key->hash; ; HASH_NEXT(j, p)) {
    conflag_dict_entry *entry = dict->table + (j & dict->mask);
    if (entry->ref.type == CONFLAG_DICT_EMPTY) {
      //printf("Key error: %s\n", key->string);
      return (conflag_ref) {CONFLAG_ERROR, {.error = CONFLAG_KEY_ERROR}};
    } else if (key->length == entry->key->length &&
        !strncmp(key->string, entry->key->string, key->length)) {
      return entry->ref;
    }
  }
}


conflag_dict *dict_new(long size) {
  conflag_dict *dict = _calloc(sizeof(conflag_dict), 1);
  if (!dict) return dict;
  if (size <= CONFLAG_MIN_DICT_SIZE) {
    *dict = (conflag_dict) {CONFLAG_MIN_DICT_TOTAL-1, 0, dict->small_table, {}};
  } else {
    long total = CONFLAG_MIN_DICT_TOTAL;
    while (total * 5 < size * 8) total <<= 1;
    conflag_dict_entry *table = _calloc(sizeof(conflag_dict_entry), total);
    if (!table) {
      free(dict);
      return NULL;
    }
    *dict = (conflag_dict) {total - 1, 0, table, {}};
  }
  for (int i = 0; i <= dict->mask; ++i) {
    dict->table[i].ref.type = CONFLAG_DICT_EMPTY;
  }
  return dict;
}


void dict_free(conflag_dict *dict) {
  for (conflag_dict_entry *e = dict->table; dict->used; ++e) {
    if (e->ref.type != CONFLAG_DICT_EMPTY) {
      _conflag_free(e->ref);
    }
  }
  if (dict->table != dict->small_table) {
    _free(dict->table);
  }
  _free(dict);
}


conflag_ast_node conflag_ast_object_new(conflag_ast_statement *statements) {
  int n = 0;
  for (conflag_ast_statement *s = statements; s; s = s->next) n++;
  conflag_ast_object *obj = dict_new(n);
  CHECK_MEM(obj);
  for (conflag_ast_statement *s = statements; s; s = s->next) {
    dict_insert(obj, s->key,
        (conflag_ref) {CONFLAG_AST_NODE, {.node = &s->value}});
  }
  return (conflag_ast_node) {CONFLAG_AST_OBJECT, {.object = obj}};
}

conflag_ast_node conflag_ast_name_new(conflag_string *name) {
  return (conflag_ast_node) {CONFLAG_AST_NAME, {.name = name}};
}

conflag_ast_node conflag_ast_reference_new(
    conflag_ast_node expression, conflag_string *reference) {
  conflag_ast_reference *ref = ALLOC(conflag_ast_reference);
  *ref = (conflag_ast_reference) {expression, reference};
  return (conflag_ast_node) {CONFLAG_AST_REFERENCE, {.reference = ref}};
}

conflag_ast_node conflag_ast_call_new(
    conflag_ast_node func, conflag_ast_nodes *args) {
  conflag_ast_call *call = ALLOC(conflag_ast_call);
  *call = (conflag_ast_call) {func, args};
  return (conflag_ast_node) {CONFLAG_AST_CALL, {.call = call}};
}

conflag_ast_node conflag_ast_number_new(char *number) {
  conflag_number_type type = CONFLAG_NUMBER_INTEGER;
  for (char *c = number; *c; ++c) {
    switch (*c) {
      case 'e':
      case 'E':
      case '.':
        type = CONFLAG_NUMBER_DOUBLE;
        break;
    }
  }
  conflag_ast_number *num = ALLOC(conflag_ast_number);
  if (type == CONFLAG_NUMBER_INTEGER) {
    *num = (conflag_ast_number) {CONFLAG_NUMBER_INTEGER, {.l = atol(number)}};
  } else {
    *num = (conflag_ast_number) {CONFLAG_NUMBER_DOUBLE, {.d = atof(number)}};
  }
  return (conflag_ast_node) {CONFLAG_AST_NUMBER, {.number = num}};
}

conflag_ast_node conflag_ast_array_new(conflag_ast_nodes *nodes) {
  return (conflag_ast_node) {CONFLAG_AST_ARRAY, {.array = nodes}};
}

conflag_ast_node conflag_ast_plus_new(conflag_ast_node a, conflag_ast_node b) {
  conflag_ast_plus *plus = ALLOC(conflag_ast_plus);
  *plus = (conflag_ast_plus) {a, b};
  return (conflag_ast_node) {CONFLAG_AST_PLUS, {.plus = plus}};
}

conflag_string *lambda_name() {
  static conflag_string underscore = {1, 0, "_"};
  static int cached = 0;
  if (!cached) underscore.hash = hash_string("_", 1);
  return &underscore;
}

conflag_ast_node conflag_ast_lambda_new(
    conflag_ast_names *args, conflag_ast_node expression) {
  conflag_ast_function *func = ALLOC(conflag_ast_function);
  *func = (conflag_ast_function) {args, lambda_name(), expression};
  return (conflag_ast_node) {CONFLAG_AST_FUNCTION, {.function = func}};
}


//TODO: These next two functions are called repeatedly during parsing
//without storing pointers to anything. Easy to leak these on OOM.
conflag_ast_nodes *conflag_ast_nodes_new(
    conflag_ast_node expression, conflag_ast_nodes *next) {
  conflag_ast_nodes *new = ALLOC(conflag_ast_nodes);
  *new = (conflag_ast_nodes) {expression, next};
  return new;
}

conflag_ast_names *conflag_ast_names_new(
    conflag_ast_node name, conflag_ast_names *next) {
  if (name.type != CONFLAG_AST_NAME) {
    THROW(CONFLAG_TYPE_ERROR, "Building arg list got unexpected type");
  }
  conflag_ast_names *new = ALLOC(conflag_ast_names);
  *new = (conflag_ast_names) {name.name, next};
  return new;
}

conflag_ast_statement *conflag_ast_statement_new(
    conflag_string *key, conflag_ast_node value) {
  conflag_ast_statement *new = ALLOC(conflag_ast_statement);
  *new = (conflag_ast_statement) {key, value, NULL};
  return new;
}

conflag_ast_statement *conflag_ast_function_new(
    conflag_string *key, conflag_ast_names *args, conflag_ast_node expression) {
  conflag_ast_function *func = _calloc(sizeof(conflag_ast_function), 1);
  conflag_ast_statement *statement = _calloc(sizeof(conflag_ast_statement), 1);
  CHECK_MEM(func, statement);
  conflag_ast_node value = {CONFLAG_AST_FUNCTION, {.function = func}};
  *func = (conflag_ast_function) {args, key, expression};
  *statement = (conflag_ast_statement) {key, value, NULL};
  return statement;
}

conflag_string *conflag_string_from_cstr(char *str, int length) {
  conflag_string *new = _calloc(length, 1);
  char *new_str = _calloc(length, 1);
  CHECK_MEM(new, new_str);
  memcpy(new_str, str, length);
  *new = (conflag_string) {length, 0, new_str};
  new->hash = hash_string(new_str, length);
  return new;
}

conflag_ref node_value(conflag_ast_node node, conflag_object *scope) {
  switch (node.type) {

    case CONFLAG_AST_OBJECT: {
      conflag_object *object = ALLOC(conflag_object);
      conflag_dict *new_scope = dict_new(node.object->used);
      CHECK_MEM(new_scope, object);
      int count = 0;
      for (conflag_dict_entry *entry = node.object->table;
           count < node.object->used; ++entry) {
        if (entry->ref.type != CONFLAG_DICT_EMPTY) {
          conflag_ref ref;
          TRY {
            ref = node_value(*entry->ref.node, object);
          } CATCH(CONFLAG_NO_MEMORY) {
            _free(object);
            dict_free(new_scope);
            THROW_OOM();
          } END_TRY;
          dict_insert(new_scope, entry->key, ref);
          ++count;
        }
      }
      *object = (conflag_object) {scope, new_scope};
      return (conflag_ref) {CONFLAG_OBJECT, {.object = object}};
    }

    case CONFLAG_AST_ARRAY: {
      conflag_array *array = ALLOC(conflag_array);
      int length = 0;
      for (conflag_ast_nodes *n = node.array; n; n = n->next) length++;
      conflag_ref *items = calloc(sizeof(conflag_ref), length);
      CHECK_MEM(items, array);
      conflag_ref *item = items;
      for (conflag_ast_nodes *n = node.array; n; n = n->next) {
        TRY {
          *(item++) = node_value(n->node, scope);
        } CATCH(CONFLAG_NO_MEMORY) {
          while (item--) _conflag_free(*item);
          _free(items);
          _free(array);
          THROW_OOM();
        } END_TRY;
      }
      *array = (conflag_array) {length, items};
      return (conflag_ref) {CONFLAG_ARRAY, {.array = array}};
    }

    case CONFLAG_AST_FUNCTION: {
      conflag_function *function = ALLOC(conflag_function);
      *function = (conflag_function) {node, scope};
      return (conflag_ref) {CONFLAG_FUNCTION, {.function = function}};
    }

    case CONFLAG_AST_CALL: {
      conflag_call *call = ALLOC(conflag_call);
      int nargs = 0;
      for (conflag_ast_nodes *n = node.call->args; n; n = n->next) nargs++;
      conflag_ref *args = calloc(sizeof(conflag_ref), nargs);
      CHECK_MEM(call, args);
      conflag_ref *arg = args;

      conflag_ref func;
      TRY {
        func = node_value(node.call->func, scope);
      } CATCH(CONFLAG_NO_MEMORY) {
        _free(args);
        _free(call);
      } END_TRY;

      for (conflag_ast_nodes *n = node.call->args; n; n = n->next) {
        TRY {
          *(arg++) = node_value(n->node, scope);
        } CATCH(CONFLAG_NO_MEMORY) {
          while (arg--) _conflag_free(*arg);
          _free(args);
          _free(call);
          _conflag_free(func);
          THROW_OOM();
        } END_TRY;
      }

      *call = (conflag_call) {nargs, 0, {0, {0}}, func, args};
      return (conflag_ref) {CONFLAG_CALL, {.call = call}};
    }

    case CONFLAG_AST_PLUS: {
      conflag_plus *plus = ALLOC(conflag_plus);
      conflag_ref a, b;

      TRY {
        a = node_value(node.plus->a, scope);
      } CATCH(CONFLAG_NO_MEMORY) {
        _free(plus);
        THROW_OOM();
      } END_TRY;

      TRY {
        b = node_value(node.plus->b, scope);
      } CATCH(CONFLAG_NO_MEMORY) {
        _free(plus);
        _conflag_free(a);
        THROW_OOM();
      } END_TRY;

      *plus = (conflag_plus) {a, b};
      return (conflag_ref) {CONFLAG_PLUS, {.plus = plus}};
    }

    case CONFLAG_AST_REFERENCE: {
      conflag_reference *reference = ALLOC(conflag_reference);
      conflag_ref obj;
      TRY {
        obj = node_value(node.reference->expression, scope);
      } CATCH(CONFLAG_NO_MEMORY) {
        _free(reference);
        THROW_OOM();
      } END_TRY;
      *reference = (conflag_reference) {obj, node.reference->reference};
      return (conflag_ref) {CONFLAG_REFERENCE, {.reference = reference}};
    }

    case CONFLAG_AST_NAME: {
      conflag_name *name = ALLOC(conflag_name);
      *name = (conflag_name) {scope, node.name};
      return (conflag_ref) {CONFLAG_NAME, {.name = name}};
    }

    case CONFLAG_AST_STRING: {
      return (conflag_ref) {CONFLAG_STRING, {.string = node.string}};
    }

    case CONFLAG_AST_NUMBER: {
      return (conflag_ref) {CONFLAG_NUMBER, {.number = node.number}};
    }
  }
  return LOGIC_ERROR;
}

conflag_ref value(conflag_ref ref) {
  switch(ref.type) {
    case CONFLAG_NAME: {
      return value(resolve(ref.name->scope, ref.name->name));
    }

    case CONFLAG_REFERENCE: {
      conflag_ref obj = value(ref.reference->expression);
      ASSERT_TYPE(obj, CONFLAG_OBJECT);
      return value(dict_get(obj.object->dict, ref.reference->name));
    }

    case CONFLAG_CALL: {
      conflag_call *call = ref.call;
      if (!call->is_cached) {
        call->cached = execute_call(call);
        call->is_cached = 1;
      }
      return call->cached;
    }

    case CONFLAG_PLUS: {
      return plus(ref.plus);
    }

    default:
      return ref;
  }
}

conflag_ref plus(const conflag_plus *plus) {
  conflag_ref a_ref = value(plus->a), b_ref = value(plus->b);
  if (a_ref.type != b_ref.type) {
    THROW(CONFLAG_TYPE_ERROR, "Can't add types: %s and %s",
        conflag_type_to_string(a_ref.type),
        conflag_type_to_string(b_ref.type));
  }
  switch (a_ref.type) {
    case CONFLAG_NUMBER: {
      conflag_number *number = ALLOC(conflag_number);
      const conflag_number *a = a_ref.number, *b = b_ref.number;
      if (a->type == CONFLAG_NUMBER_INTEGER && a->type == b->type) {
          *number = (conflag_number)
              {CONFLAG_NUMBER_INTEGER, {.l = a->l + b->l}};
      } else {
        double ad = (a->type == CONFLAG_NUMBER_INTEGER? a->l : a->d),
               bd = (b->type == CONFLAG_NUMBER_INTEGER? b->l : b->d);
        *number = (conflag_number) {CONFLAG_NUMBER_DOUBLE, {.d = ad + bd}};
      }
      return (conflag_ref) {CONFLAG_NUMBER, {.number = number}};
    }
    case CONFLAG_STRING: {
      const conflag_string *a = a_ref.string, *b = b_ref.string;
      conflag_string *string = conflag_string_from_cstr(
          a->string, a->length + b->length);
      memcpy(string->string + a->length, b->string, b->length);
      string->hash = hash_string(string->string, string->length);
      return (conflag_ref) {CONFLAG_STRING, {.string = string}};
    }
    case CONFLAG_OBJECT: {
      conflag_object *object = ALLOC(conflag_object);
      const conflag_object *a = a_ref.object, *b = b_ref.object;
      //TODO: This will overallocate duplicates.
      object->dict = dict_new(a->dict->used + b->dict->used);
      CHECK_MEM(object->dict, object);
      int count = 0;
      for (conflag_dict_entry *e = a->dict->table; count < a->dict->used; e++) {
        if (e->ref.type != CONFLAG_DICT_EMPTY) {
          dict_insert(object->dict, e->key, e->ref);
          count++;
        }
      }
      count = 0;
      for (conflag_dict_entry *e = b->dict->table; count < b->dict->used; e++) {
        if (e->ref.type != CONFLAG_DICT_EMPTY) {
          dict_insert(object->dict, e->key, e->ref);
          count++;
        }
      }
      return (conflag_ref) {CONFLAG_OBJECT, {.object = object}};
    }
    case CONFLAG_ARRAY: {
      conflag_array *array = ALLOC(conflag_array);
      const conflag_array *a = a_ref.array, *b = b_ref.array;
      array->array = _calloc(sizeof(conflag_ref), a->length + b->length);
      CHECK_MEM(array->array, array);
      memcpy(array->array, a->array, a->length * sizeof(conflag_ref));
      memcpy(array->array + a->length, b->array,
          b->length * sizeof(conflag_ref));
      array->length = a->length + b->length;
      return (conflag_ref) {CONFLAG_ARRAY, {.array = array}};
    }

    default:
      THROW(CONFLAG_TYPE_ERROR, "Cannot add %s types",
          conflag_type_to_string(a_ref.type));
  }
  return LOGIC_ERROR;
}

conflag_ref to_conflag_bool(int b) {
  return b ? CONFLAG_TRUE : CONFLAG_FALSE;
}

int equals(conflag_ref a, conflag_ref b) {
  a = value(a);
  b = value(b);
  if (a.type != b.type) return 0;
  switch (a.type) {
    case CONFLAG_STRING: {
      if (a.string->length != b.string->length) return 0;
      return !strncmp(a.string->string, b.string->string, a.string->length);
    }

    case CONFLAG_NUMBER: {
      if (a.number->type != b.number->type) return 0;
      if (a.number->type == CONFLAG_NUMBER_INTEGER) {
        return a.number->l == b.number->l;
      } else {
        return a.number->d == b.number->d;
      }
    }

    case CONFLAG_BOOLEAN: {
      return a.boolean == b.boolean;
    }

    case CONFLAG_NULL_TYPE: {
      return 1;
    }

    case CONFLAG_ARRAY: {
      if (a.array->length != b.array->length) return 0;
      for (int i = 0; i < a.array->length; ++i) {
        if (!equals(a.array->array[i], b.array->array[i])) {
          return 0;
        }
      }
      return 1;
    }

    case CONFLAG_OBJECT: {
      if (a.object->dict->used != b.object->dict->used) return 0;
      int count = 0;
      for (conflag_dict_entry *e = a.object->dict->table;
          count < a.object->dict->used; ++e) {
        if (e->ref.type != CONFLAG_DICT_EMPTY) {
          ++count;
          conflag_ref r2 = dict_get(b.object->dict, e->key);
          if (!equals(e->ref, r2)) {
            return 0;
          }
        }
      }
      return 1;
    }

    case CONFLAG_FUNCTION: {
      return a.function == b.function;
    }

    case CONFLAG_NATIVE_FUNCTION: {
      return a.native_function == b.native_function;
    }

    default:
      return 0;
  }
}

conflag_ref execute_call(conflag_call *call) {
  conflag_ref func_ref = value(call->function);
  if (func_ref.type == CONFLAG_NATIVE_FUNCTION) {
    return execute_native_call(func_ref.native_function, call);
  }
  ASSERT_TYPE(func_ref, CONFLAG_FUNCTION);
  const conflag_function *func = func_ref.function;
  int num_args = 0;
  for (conflag_ast_names *n = func->node.function->args; n; n = n->next) {
    num_args++;
  }
  if (num_args != call->num_args) {
    THROW(CONFLAG_TYPE_ERROR, "%s expects %d arguments, got %d",
        func->node.function->name->string, num_args, call->num_args);
  }

  conflag_object *call_scope = ALLOC(conflag_object);
  call_scope->parent = func->scope;
  call_scope->dict = dict_new(call->num_args);
  CHECK_MEM(call_scope, call_scope->dict);
  conflag_ref *arg = call->args;
  for (conflag_ast_names *n = func->node.function->args; n; n = n->next) {
    conflag_ref argval;
    TRY {
      argval = value(*(arg++));
    } CATCH_ALL {
      //TODO: I also think this just isn't going to work. Gonna need ref counts.
      dict_free(call_scope->dict);
      _free(call_scope);
      //TODO: Macro magic to make this work with no args
      THROW(EXCEPTION, "%s", error_msg);
    } END_TRY;
    dict_insert(call_scope->dict, n->name, argval);
  }
  //TODO: Worry about errors
  return value(node_value(func->node.function->expression, call_scope));
}

conflag_ref execute_native_call(
    conflag_native_function const* func, conflag_call *call) {
  if (call->num_args != func->num_args) {
    THROW(CONFLAG_TYPE_ERROR, "%s expects %d arguments, got %d",
        func->name, func->num_args, call->num_args);
  }
  conflag_ref args[call->num_args];
  for (int i = 0; i < call->num_args; ++i) {
    args[i] = call->args[i];
  }
  return value(func->func(args));
}

conflag_ref resolve(conflag_object *object, conflag_string *name) {
  for (conflag_object *o = object; o; o = o->parent) {
    conflag_ref ref = dict_get(o->dict, name);
    if (ref.type != CONFLAG_ERROR || ref.error != CONFLAG_KEY_ERROR) {
      return ref;
    }
  }
  //TODO: Make this not an exception case
  THROW(CONFLAG_NAME_RESOLUTION_ERROR,
      "Couldn't resolve name '%s'", name->string);
  return LOGIC_ERROR;
}

conflag_ref conflag_load(char *filename) {
  FILE *f = fopen(filename, "r");
  yyin = f;
  while (!feof(yyin)) {
    yyparse();
  }
  printf("done parsing\n");
  printf("Type: %x\n", conflag_parse_output.type);
  TRY {
    return node_value(conflag_parse_output, builtin_scope());
  } CATCH_ALL {
    return (conflag_ref) {CONFLAG_ERROR, {.error = EXCEPTION}};
  } END_TRY;
  return LOGIC_ERROR;
}

void _conflag_free(conflag_ref ref) {
  return; //TODO
}



conflag_ref builtin_map(conflag_ref *args) {
  // We verify that the correct number of args were passed in execute_call
  conflag_ref func = value(args[0]),
              array = value(args[1]);
  array = value(array);
  ASSERT_TYPE(func, CONFLAG_FUNCTION);
  ASSERT_TYPE(array, CONFLAG_ARRAY);
  conflag_array *out = ALLOC(conflag_array);
  out->array = _calloc(sizeof(conflag_ref), array.array->length);
  out->length = array.array->length;
  CHECK_MEM(out->array, out);
  conflag_ref *ri = array.array->array, *ro = out->array;
  for (int i = 0; i < out->length; ++i) {
    conflag_call call = (conflag_call) {1, 0, {0, {0}}, func, ri++};
    *(ro++) = execute_call(&call);
  }
  return (conflag_ref) {CONFLAG_ARRAY, {.array = out}};
}

conflag_ref builtin_reduce(conflag_ref *args) {
  conflag_ref func = value(args[0]),  // Cache a simple type
              array = value(args[1]),
              base = args[2];
  ASSERT_TYPE(array, CONFLAG_ARRAY);
  conflag_ref result = value(base);
  for (int i = 0; i < array.array->length; ++i) {
    conflag_ref args[2] = {result, array.array->array[i]};
    conflag_call call = {2, 0, {0, {0}}, func, args};
    result = execute_call(&call);
  }
  return result;
}

conflag_ref builtin_import(conflag_ref *args) {
  conflag_ref location = value(args[0]);
  ASSERT_TYPE(location, CONFLAG_STRING);
  return conflag_load(location.string->string);
}

conflag_ref builtin_equals(conflag_ref *args) {
  return to_conflag_bool(equals(args[0], args[1]));
}

//TODO: Use a macro to define these that takes in the number of args
conflag_ref builtin_if(conflag_ref *args) {
  conflag_ref cond = value(args[0]),
              a = args[1],
              b = args[2];
  ASSERT_TYPE(cond, CONFLAG_BOOLEAN);
  return cond.boolean ? value(a) : value(b);
}

//TODO: Turn this into a header etc.
struct builtin { char *key; conflag_ref value; };

#define CONFLAG_FUNCTION_DEF(name, func, argc) {name, {CONFLAG_NATIVE_FUNCTION,\
  {.native_function = &((conflag_native_function){name, argc, func})}}}
#define END_MODULE {NULL, {0, {0}}}

#define CONFLAG_BOOLEAN_DEF(name, value) \
   {name, {CONFLAG_BOOLEAN, {.boolean = value}}}

const struct builtin builtins[] = {
  CONFLAG_FUNCTION_DEF("map", builtin_map, 2),
  CONFLAG_FUNCTION_DEF("reduce", builtin_reduce, 3),
  CONFLAG_FUNCTION_DEF("import", builtin_import, 1),
  CONFLAG_FUNCTION_DEF("equals", builtin_equals, 2),
  CONFLAG_FUNCTION_DEF("if", builtin_if, 3),
  CONFLAG_BOOLEAN_DEF("true", 1),
  CONFLAG_BOOLEAN_DEF("false", 0),
  {"null", {CONFLAG_NULL_TYPE, {0}}},
  END_MODULE
};


conflag_object *import_native(struct builtin const* natives) {
  conflag_object *scope = ALLOC(conflag_object);
  int num_builtins = 0;
  for (struct builtin const* b = natives; b->key; ++b) num_builtins++;
  scope->dict = dict_new(num_builtins);
  CHECK_MEM(scope->dict, scope);
  scope->parent = NULL;
  for (int i = 0; i < num_builtins; ++i) {
    struct builtin b = natives[i];
    dict_insert(scope->dict,
        conflag_string_from_cstr(b.key, strlen(b.key)), b.value);
  }
  return scope;
}


conflag_object *builtin_scope() {
  static conflag_object *cached = NULL;
  if (!cached) {
    cached = import_native(builtins);
  }
  return cached;
}


char *conflag_type_to_string(conflag_type type) {
  switch(type) {
    case CONFLAG_NULL_TYPE: return "null";
    case CONFLAG_AST_NODE: return "ast node";
    case CONFLAG_OBJECT: return "object";
    case CONFLAG_ARRAY: return "array";
    case CONFLAG_FUNCTION: return "function";
    case CONFLAG_CALL: return "call";
    case CONFLAG_PLUS: return "plus";
    case CONFLAG_REFERENCE: return "reference";
    case CONFLAG_NAME: return "name";
    case CONFLAG_STRING: return "string";
    case CONFLAG_NUMBER: return "number";
    case CONFLAG_BOOLEAN: return "boolean";
    case CONFLAG_NATIVE_FUNCTION: return "native function";
    case CONFLAG_DICT_EMPTY: return "DICT_EMPTY";
    case CONFLAG_ERROR: return "error";
    default: return NULL;
  }
}


int is_valid_identifier(conflag_string *string) {
  if (!string->length) return 0;
  char *str = string->string;
  #define is_alphanumeric(c) ((c >= 'a' && c <= 'z') || \
                              (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))
  if (!is_alphanumeric(*str)) return 0;
  for (int i = 1; i < string->length; ++i) {
    if (!is_alphanumeric(str[i]) && str[i] != '_') return 0;
  }
  return 1;
}


void conflag_ref_print_(conflag_ref ref, short indent,
    int indent_level, int maybe_parens) {
  if (indent) {
    for (int i = 0; i < indent_level; ++i) {
      printf("  ");
    }
  }
  switch(ref.type) {
    case CONFLAG_STRING:
      printf("\"%s\"", ref.string->string); break;
    case CONFLAG_NUMBER:
      switch (ref.number->type) {
        case CONFLAG_NUMBER_DOUBLE:
          printf("%lf", ref.number->d); break;
        case CONFLAG_NUMBER_INTEGER:
          printf("%ld", ref.number->l); break;
      }; break;
    case CONFLAG_BOOLEAN:
      printf("%s", ref.boolean ? "true" : "false"); break;
    case CONFLAG_ERROR:
      printf("Error(%d)", ref.error); break;
    case CONFLAG_ARRAY:
      if (!ref.array->length) {
        printf("[]");
      } else if (ref.array->length == 1) {
        printf("[");
        conflag_ref_print_(ref.array->array[0], 0, indent_level + 1, 0);
        printf("]");
      } else {
        printf("[\n");
        for (int i = 0; i < ref.array->length; ++i) {
          conflag_ref_print_(ref.array->array[i], 1, indent_level + 1, 0);
          printf(",\n");
        };
        for (int i = 0; i < indent_level; ++i) printf("  ");
        printf("]");
      }
      break;
    case CONFLAG_OBJECT:
      printf("{\n");
      int count = 0;
      for (conflag_dict_entry *e = ref.object->dict->table;
          count < ref.object->dict->used; ++e) {
        if (e->ref.type != CONFLAG_DICT_EMPTY) {
          count++;
          for (int i = 0; i < indent_level + 1; ++i) printf("  ");
          if (is_valid_identifier(e->key)) {
            printf("%s", e->key->string);
          } else {
            printf("\"%s\"", e->key->string);
          }
          if (e->ref.type == CONFLAG_FUNCTION) {
            conflag_ast_function *f = e->ref.function->node.function;
            for (conflag_ast_names *n = f->args; n; n = n->next) {
              printf(" %s", n->name->string);
            }
            printf(": ");
            conflag_ref_print_(node_value(f->expression, NULL),
                0, indent_level + 1, 0);
          } else {
            printf(": ");
            conflag_ref_print_(e->ref, 0, indent_level + 1, 0);
          }
          printf(",\n");
        }
      }
      for (int i = 0; i < indent_level; ++i) printf("  ");
      printf("}");
      break;
    case CONFLAG_FUNCTION: {
      conflag_ast_function *f = ref.function->node.function;
      printf("(_");
      for (conflag_ast_names *n = f->args; n; n = n->next) {
        printf(" %s", n->name->string);
      }
      printf(": ");
      conflag_ref_print_(node_value(f->expression, NULL),
          0, indent_level + 1, 0);
      printf(")");
      break;
    }
    case CONFLAG_NATIVE_FUNCTION:
      printf("<native function %s>", ref.native_function->name);
      break;
    case CONFLAG_PLUS:
      if (maybe_parens) printf("(");
      conflag_ref_print_(ref.plus->a, 0, indent_level, 1);
      printf(" + ");
      conflag_ref_print_(ref.plus->b, 0, indent_level, 1);
      if (maybe_parens) printf(")");
      break;
    case CONFLAG_CALL:
      if (maybe_parens) printf("(");
      conflag_ref_print_(ref.call->function, 0, indent_level, 1);
      for (int i = 0; i < ref.call->num_args; ++i) {
        printf(" ");
        conflag_ref_print_(ref.call->args[i], 0, indent_level, 1);
      }
      if (maybe_parens) printf(")");
      break;
    case CONFLAG_NAME:
      printf("%s", ref.name->name->string);
      break;
    case CONFLAG_REFERENCE:
      conflag_ref_print_(ref.reference->expression, 0, indent_level, 1);
      conflag_string *name = ref.reference->name;
      printf(is_valid_identifier(name) ? ".%s" : ".\"%s\"", name->string);
      break;
    default:
      printf("Unprintable type %s", conflag_type_to_string(ref.type));
  }
}

void conflag_ref_print(conflag_ref ref) {
  conflag_ref_print_(ref, 0, 0, 0);
  printf("\n");
}

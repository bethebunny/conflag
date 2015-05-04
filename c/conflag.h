#ifndef __conflag_h

#define CONFLAG_MIN_DICT_SIZE 5
#define CONFLAG_MIN_DICT_TOTAL 8

// Needed by several files related to parsing/lexing; these should be moved.
// Needed for FILE
#include <stdio.h>

int parse_error(char *s);
int yylex();
int yyparse();
FILE *yyin;


typedef enum {
  CONFLAG_NULL_TYPE,
  CONFLAG_AST_NODE,
  CONFLAG_OBJECT,
  CONFLAG_ARRAY,
  CONFLAG_FUNCTION,
  CONFLAG_CALL,
  CONFLAG_PLUS,
  CONFLAG_REFERENCE,
  CONFLAG_NAME,
  CONFLAG_STRING,
  CONFLAG_NUMBER,
  CONFLAG_BOOLEAN,
  CONFLAG_NATIVE_FUNCTION,
  CONFLAG_DICT_EMPTY,
  CONFLAG_ERROR,
} conflag_type;

typedef enum {
  CONFLAG_NUMBER_DOUBLE,
  CONFLAG_NUMBER_INTEGER,
} conflag_number_type;

//TODO: Full error type with debug info

typedef enum {
  CONFLAG_NO_MEMORY,
  CONFLAG_TYPE_ERROR,
  CONFLAG_KEY_ERROR,
  CONFLAG_LOGIC_ERROR,
  CONFLAG_SYNTAX_ERROR,
  CONFLAG_NAME_RESOLUTION_ERROR,
} conflag_error_type;

typedef enum {
  CONFLAG_AST_OBJECT,
  CONFLAG_AST_ARRAY,
  CONFLAG_AST_FUNCTION,
  CONFLAG_AST_CALL,
  CONFLAG_AST_PLUS,
  CONFLAG_AST_REFERENCE,
  CONFLAG_AST_NAME,
  CONFLAG_AST_STRING,
  CONFLAG_AST_NUMBER,
} conflag_ast_node_type;

typedef struct conflag_dict conflag_dict;
typedef struct conflag_dict_entry conflag_dict_entry;

typedef struct conflag_array conflag_array;
typedef struct conflag_call conflag_call;
typedef struct conflag_function conflag_function;
typedef struct conflag_name conflag_name;
typedef struct conflag_number conflag_number;
typedef struct conflag_object conflag_object;
typedef struct conflag_plus conflag_plus;
typedef struct conflag_reference conflag_reference;
typedef struct conflag_string conflag_string;
typedef struct conflag_native_function conflag_native_function;

typedef struct conflag_ref conflag_ref;

typedef struct conflag_ast_call conflag_ast_call;
typedef struct conflag_ast_function conflag_ast_function;
typedef struct conflag_ast_names conflag_ast_names;
typedef struct conflag_ast_node conflag_ast_node;
typedef struct conflag_ast_nodes conflag_ast_nodes;
typedef struct conflag_ast_plus conflag_ast_plus;
typedef struct conflag_ast_reference conflag_ast_reference;
typedef struct conflag_ast_statement conflag_ast_statement;

typedef conflag_ast_nodes conflag_ast_array;
typedef conflag_string conflag_ast_name;
typedef conflag_number conflag_ast_number;
typedef conflag_dict conflag_ast_object;


struct conflag_string {
  int length;
  long hash;
  char *string;
};

struct conflag_number {
  conflag_number_type type;
  union {
    double d;
    long l;
  };
};


struct conflag_ref {
  conflag_type type;
  union {
    conflag_ast_node *node;
    conflag_array *array;
    conflag_call *call;
    conflag_function *function;
    conflag_name *name;
    conflag_number *number;
    conflag_object *object;
    conflag_plus *plus;
    conflag_reference *reference;
    conflag_string *string;
    const conflag_native_function *native_function;
    conflag_error_type error;
    short boolean;
  };
};


extern const conflag_ref CONFLAG_NULL;
extern const conflag_ref CONFLAG_TRUE;
extern const conflag_ref CONFLAG_FALSE;


struct conflag_dict_entry{
  conflag_string *key;
  conflag_ref ref;
};

struct conflag_dict {
  long mask; // Current allocated size - 1
  long used; // # active
  conflag_dict_entry *table;
  conflag_dict_entry small_table[CONFLAG_MIN_DICT_TOTAL];
};


struct conflag_ast_node {
  conflag_ast_node_type type;
  union {
    conflag_ast_array *array;
    conflag_ast_call *call;
    conflag_ast_function *function;
    conflag_ast_name *name;
    conflag_ast_number *number;
    conflag_ast_object *object;
    conflag_ast_plus *plus;
    conflag_ast_reference *reference;
    conflag_string *string;
  };
};


struct conflag_ast_call {
  conflag_ast_node func;
  conflag_ast_nodes *args;
};

struct conflag_ast_function {
  conflag_ast_names *args;
  conflag_string *name;
  conflag_ast_node expression;
};

struct conflag_ast_names {
  conflag_string *name;
  conflag_ast_names *next;
};

struct conflag_ast_nodes {
  conflag_ast_node node;
  conflag_ast_nodes *next;
};

struct conflag_ast_plus {
  conflag_ast_node a;
  conflag_ast_node b;
};

struct conflag_ast_reference {
  conflag_ast_node expression;
  conflag_string *reference;
};

struct conflag_ast_statement {
  conflag_string *key;
  conflag_ast_node value;
  conflag_ast_statement *next;
};


struct conflag_array {
  long length;
  conflag_ref *array;
};

struct conflag_call {
  short num_args;
  short is_cached;
  conflag_ref cached;
  conflag_ref function;
  conflag_ref *args;
};

struct conflag_function {
  conflag_ast_node node;
  conflag_object *scope;
};

struct conflag_native_function {
  char *name;
  short num_args;
  conflag_ref (*func)(conflag_ref *args);
};

struct conflag_name {
  conflag_object *scope;
  conflag_string *name;
};

struct conflag_object {
  conflag_object *parent;
  conflag_dict *dict;
};

struct conflag_plus {
  conflag_ref a;
  conflag_ref b;
};

struct conflag_reference {
  conflag_ref expression;
  conflag_string *name;
};


conflag_ref conflag_load(char *filename);
conflag_ref conflag_load_file(FILE *file);

int get_long(conflag_object *cfg, char *query, long *value);
int get_double(conflag_object *cfg, char *query, double *value);
int get_string(conflag_object *cfg, char *query, char **value);
int get_boolean(conflag_object *cfg, char *query, int *value);
conflag_ref get(conflag_object *cfg, char *query);

conflag_ast_node conflag_ast_object_new(conflag_ast_statement *statements);
conflag_ast_node conflag_ast_name_new(conflag_string *name);
conflag_ast_node conflag_ast_reference_new(
    conflag_ast_node expression, conflag_string *reference);
conflag_ast_node conflag_ast_call_new(
    conflag_ast_node func, conflag_ast_nodes *args);
conflag_ast_node conflag_ast_number_new(char *number);
conflag_ast_node conflag_ast_array_new(conflag_ast_nodes *nodes);
conflag_ast_node conflag_ast_plus_new(conflag_ast_node a, conflag_ast_node b);
conflag_ast_node conflag_ast_lambda_new(
    conflag_ast_names *args, conflag_ast_node expression);

conflag_ast_nodes *conflag_ast_nodes_new(
    conflag_ast_node expression, conflag_ast_nodes *next);
conflag_ast_names *conflag_ast_names_new(
    conflag_ast_node name, conflag_ast_names *next);

conflag_ast_statement *conflag_ast_statement_new(
    conflag_string *key, conflag_ast_node value);
conflag_ast_statement *conflag_ast_function_new(
    conflag_string *key, conflag_ast_names *args, conflag_ast_node expression);

conflag_string *conflag_string_from_cstr(char *str, int length);

char *conflag_type_to_string(conflag_type type);
void conflag_ref_print(conflag_ref ref);

void conflag_set_allocators(
    void* (*calloc)(size_t, size_t), void (*free)(void*));

conflag_ast_node conflag_parse_output;

#endif /* __conflag_h */

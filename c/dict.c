#include "dict.h"
#include <string.h>
#include <stdlib.h>

#define PERTURB_BITS 5
#define HASH_NEXT(j, p) j = (5 * j) + 1 + p, p >>= PERTURB_BITS


static void* (*_calloc)(size_t, size_t) = calloc;
static void (*_free)(void*) = free;

void conflag_dict_set_allocators(
    void * (*calloc)(size_t, size_t), void (*free)(void*)) {
  _calloc = calloc;
  _free = free;
}

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


void conlfag_dict_insert(
    conflag_dict *dict, conflag_string *key, conflag_ref value) {
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

conflag_ref conflag_dict_get(conflag_dict *dict, conflag_string *key) {
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
conflag_dict *conflag_dict_new(long size) {
  conflag_dict *dict = _calloc(sizeof(conflag_dict), 1);
  if (!dict) return dict;
  if (size <= CONFLAG_MIN_DICT_SIZE) {
    *dict = (conflag_dict) {CONFLAG_MIN_DICT_TOTAL-1, 0, dict->small_table, {}};
  } else {
    long total = CONFLAG_MIN_DICT_TOTAL;
    while (total * 5 < size * 8) total <<= 1;
    conflag_dict_entry *table = _calloc(sizeof(conflag_dict_entry), total);
    if (!table) {
      _free(dict);
      return NULL;
    }
    *dict = (conflag_dict) {total - 1, 0, table, {}};
  }
  for (int i = 0; i <= dict->mask; ++i) {
    dict->table[i].ref.type = CONFLAG_DICT_EMPTY;
  }
  return dict;
}


void conflag_dict_free(conflag_dict *dict) {
  for (conflag_dict_entry *e = dict->table; dict->used; ++e) {
    if (e->ref.type != CONFLAG_DICT_EMPTY) {
      conflag_free(e->ref);
    }
  }
  if (dict->table != dict->small_table) {
    _free(dict->table);
  }
  _free(dict);
}

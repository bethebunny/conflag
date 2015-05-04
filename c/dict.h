#ifndef __conflag_dict_h
#define __conflag_dict_h

#define CONFLAG_MIN_DICT_SIZE 5
#define CONFLAG_MIN_DICT_TOTAL 8

#include "string.h"
#include "conflag.h"

typedef struct conflag_dict conflag_dict;
typedef struct conflag_dict_entry conflag_dict_entry;

struct conflag_dict;
struct conflag_dict_entry;

conflag_ref conflag_dict_get(conflag_dict *dict, conflag_string *key);
void conflag_dict_insert(
    conflag_dict *dict, conflag_string *key, conflag_ref value);
conflag_dict *conflag_dict_new(long size);
void conflag_dict_free(conflag_dict *dict);

void conflag_dict_set_allocators(
    void * (*calloc)(size_t, size_t), void (*free)(void*));

#endif /* __conflag_dict_h */

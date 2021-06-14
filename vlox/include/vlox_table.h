#ifndef __VLOX_TABLE_H
#define __VLOX_TABLE_H

#include "vlox_common.h"
#include "vlox_value.h"

typedef struct {
    ObjString* key;
    Value value;
} Entry;


typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);
bool tableSet(Table* table, ObjString* key, Value value);
bool tableGet(Table* table, ObjString* key, Value* value);
bool tableDelete(Table* table, ObjString* key);
void tableAddAll(Table* from, Table* to);
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);

#endif // __VLOX_TABLE_H
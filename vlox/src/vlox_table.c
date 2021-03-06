#include <stdlib.h>
#include <string.h>

#include "vlox_memory.h"
#include "vlox_object.h"
#include "vlox_table.h"
#include "vlox_value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    // uint32_t index = key->hash % capacity;
    uint32_t index = key->hash & (capacity - 1);
    Entry* tombstone = NULL;
    
    for(;;) {
        Entry* entry = &entries[index];
        
        if(entry->key == NULL) {
            // check if its tombstone.
            if(IS_NIL(entry->value)) {
                // not a tombstone. reached DEAD END. can stop probing.
                // if already found a tombstone, we use it for new entry,
                // else send this NULL entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // a tombstone.
                if(tombstone == NULL) tombstone = entry;
            }
        } else if(entry->key == key) {
            // found the entry.
            return entry;
        }

        // index = (index+1)%capacity;
        index = (index + 1) & (capacity - 1);
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if(table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if(entry == NULL) return false;

    *value = entry->value;
    return true;
}

static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);
    for(int i=0; i<capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    table->count = 0;
    for(int i=0; i<table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if(entry->key == NULL) continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool tableSet(Table* table, ObjString* key, Value value) {

    if(table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool is_new_key = entry->key == NULL;

    // only increase count if key is new and being stored in a new entry
    // instead of a tombstone.
    if(is_new_key && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return is_new_key;
}

bool tableDelete(Table* table, ObjString* key) {
    if(table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if(entry->key == NULL) return false;

    // Place a tombstone in the entry. Lets next delete probe to continue probing.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(Table* from, Table* to) {
    for(int i=0; i<from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if(entry->key != NULL)  {
            tableSet(to, entry->key, entry->value);
        }
    }
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if(table->count == 0) return NULL;

    // uint32_t index = hash%(table->capacity);
    uint32_t index = hash & (table->capacity - 1);
    for(;;) {
        Entry* entry = &table->entries[index];

        if(entry->key == NULL) {
            // stop if we find an empty non-tombstone entry
            if(IS_NIL(entry->value)) return NULL;
        } else if(entry->key->length == length && entry->key->hash == hash 
                && memcmp(entry->key->chars, chars, length) == 0) {
                    // we found it.
                    return entry->key;
        }

        // index = (index + 1) % (table->capacity);
        index = (index + 1) & (table->capacity - 1);
    }
}


void markTable(Table* table) {
    for(int i=0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}

void tableRemoveWhite(Table* table) {
    for(int i=0; i<table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if(entry->key != NULL && !entry->key->obj.is_marked) {
            tableDelete(table, entry->key);
        }
    }
}
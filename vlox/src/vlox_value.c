#include <stdio.h>
#include <string.h>

#include "vlox_memory.h"
#include "vlox_value.h"
#include "vlox_object.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->count  = 0;
    array->capacity = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    if(array->capacity < array->count+1) {
        int old_capacity = array->capacity;
        array->capacity = GROW_CAPACITY(old_capacity);
        array->values = GROW_ARRAY(Value, array->values, old_capacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
    switch(value.type) {
        case VAL_BOOL:  
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        
        case VAL_NUMBER:
            printf("%g", AS_NUMBER(value));
            break;

        case VAL_OBJ:
            printObject(value); 
            break;

        case VAL_NIL:
            printf("nil");
            break;
    }
}

bool valuesEqual(Value a, Value b) {
    if(a.type != b.type)    return false;
    switch(a.type) {
        case VAL_BOOL:      return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:       return true;
        case VAL_NUMBER:    return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ:   {
            ObjString* a_str = AS_STRING(a);
            ObjString* b_str = AS_STRING(b);
            return (a_str->length == b_str->length)
                    && (memcmp(a_str->chars, b_str->chars, a_str->length) == 0);
        }
        default:            return false;  // unreachable
    }
}


void printObject(Value value) {
    switch (OBJ_TYPE(value))
    {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    
        default:
            break;
    }
}
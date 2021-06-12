#ifndef __VLOX_VALUE_H
#define __VLOX_VALUE_H

#include "vlox_common.h"

#define STACK_MAX 256

typedef double Value;

typedef struct {
    int count;
    int capacity;
    Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif // __VLOX_VALUE_H
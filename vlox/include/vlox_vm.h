#ifndef __VLOX_VM_H
#define __VLOX_VM_H

#include "vlox_chunk.h"
#include "vlox_value.h"
#include "vlox_object.h"
#include "vlox_table.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjFunction* function;
    uint8_t* ip;    // points to the next instruction to be executed after a function is called.
    Value* slots;   // points to the first slot given for function in stack
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frame_count;

    Value stack[STACK_MAX]; // stack of this stack based VM
    Value* stackTop;    // points to top_index+1. If empty, points to zeroth index.
    Obj* objects;       // pointer to the head of all heap allocated objects linked list
    Table strings_pool;      // to store all unique strings.
    Table globals;          // to store all global variables.
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif // __VLOX_VM_H
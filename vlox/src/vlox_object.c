#include <stdio.h>
#include <string.h>

#include "vlox_memory.h"
#include "vlox_object.h"
#include "vlox_value.h"
#include "vlox_vm.h"
#include "vlox_table.h"

#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

static ObjString* allocateString(char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    tableSet(&vm.strings_pool, string, NIL_VAL);
    return string;
}

// FNV-1a hash algo. - http://www.isthe.com/chongo/tech/comp/fnv/
static uint32_t hashString(const char* key, int length) {

    uint32_t hash = 2166136261u;
    for(int i=0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }

    return hash;
}

ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);

    // if string is already present in string pool, sent that key.
    ObjString* interned = tableFindString(&vm.strings_pool, chars, length, hash);
    if(interned != NULL) return interned;

    char* heap_chars = ALLOCATE(char, length+1);
    memcpy(heap_chars, chars, length);
    heap_chars[length] = '\0';
    return allocateString(heap_chars, length, hash);
}

// copyString assumes a new string needs to be created from given char array
// takeString creates string object from given char array.
ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);

    ObjString* interned = tableFindString(&vm.strings_pool, chars, length, hash);
    if(interned != NULL) {
        FREE_ARRAY(char, chars, length+1);
        return interned;
    }

    return allocateString(chars, length, hash);
}

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalue_count = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjNative* newNative(NativeFn  function) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

ObjClosure* newClosure(ObjFunction* function) {

    ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalue_count);
    for(int i=0; i < function->upvalue_count; i++) {
        upvalues[i] = NULL;
    }

    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalue_count = function->upvalue_count;
    return closure;
}

ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NIL_VAL;
    upvalue->location = slot;
    upvalue->next = NULL;
    return upvalue;
}

static void printFunction(ObjFunction* function) {
    if(function->name==NULL) {
        printf("<script>");
        return;
    }
    printf("<fn %s>", function->name->chars);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value))
    {
        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
            break;

        case OBJ_NATIVE:
            printf("<native fn>");
            break;

        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;

        case OBJ_CLOSURE:
            printFunction(AS_CLOSURE(value)->function);
            break;

        case OBJ_UPVALUE:
            printf("upvalue");
            break;
    
        default:
            break;
    }
}
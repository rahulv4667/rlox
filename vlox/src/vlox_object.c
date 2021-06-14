#include <stdio.h>
#include <string.h>

#include "vlox_memory.h"
#include "vlox_object.h"
#include "vlox_value.h"
#include "vlox_vm.h"

#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

static ObjString* allocateString(char* chars, int length) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    return string;
}

ObjString* copyString(const char* chars, int length) {
    char* heap_chars = ALLOCATE(char, length+1);
    memcpy(heap_chars, chars, length);
    heap_chars[length] = '\0';
    return allocateString(heap_chars, length);
}

ObjString* takeString(char* chars, int length) {
    return allocateString(chars, length);
}
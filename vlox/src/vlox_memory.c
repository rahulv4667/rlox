#include <stdlib.h>
#include "vlox_memory.h"
#include "vlox_vm.h"
#include "vlox_object.h"

void* reallocate(void* pointer, size_t old_size, size_t new_size) {
    if(new_size == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, new_size);
    if(result == NULL)
        exit(EXIT_FAILURE);
    return result;
}

static void freeObject(Obj* object) {
    switch(object->type) {
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        
        case OBJ_NATIVE: {
            FREE(ObjNative, object);
            break;
        }

        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalue_count);
            FREE(ObjClosure, object);
            break;
        }

        case OBJ_UPVALUE: {
            FREE(ObjUpvalue, object);
            break;
        }

        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
    }
}

void freeObjects() {
    Obj* object = vm.objects;
    while(object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
}
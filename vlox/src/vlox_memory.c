#include <stdlib.h>
#include "vlox_memory.h"
#include "vlox_vm.h"
#include "vlox_object.h"
#include "vlox_common.h"
#include "vlox_compiler.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "vlox_debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* pointer, size_t old_size, size_t new_size) {

    vm.bytes_allocated += (new_size - old_size);

    if(new_size > old_size) {
        #ifdef DEBUG_STRESS_GC
            collectGarbage();
        #endif

        if(vm.bytes_allocated > vm.next_GC) {
            collectGarbage();
        }
    }

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

    #ifdef DEBUG_LOG_GC
        printf("%p free type %d\n", (void*)object, object->type);
    #endif

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

        case OBJ_CLASS: {
            FREE(ObjClass, object);
            break;
        }

        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            freeTable(&instance->fields);
            FREE(ObjInstance, object);
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

    free(vm.gray_stack);
}


void markObject(Obj* object) {
    if(object == NULL) return;
    if(object->is_marked) return;

    #ifdef DEBUG_LOG_GC
        printf("%p mark ", (void*)object);
        printValue(OBJ_VAL(object));
        printf("\n");
    #endif

    object->is_marked = true;

    if(vm.gray_capacity < vm.gray_count + 1) {
        vm.gray_capacity = GROW_CAPACITY(vm.gray_capacity);
        vm.gray_stack = (Obj**)realloc(vm.gray_stack, sizeof(Obj*) * vm.gray_capacity);

        if(vm.gray_stack == NULL) exit(EXIT_FAILURE);
    }

    vm.gray_stack[vm.gray_count++] = object;
}

void markValue(Value value) {
    if(IS_OBJ(value)) markObject(AS_OBJ(value));
}

static void markRoots() {
    for(Value* slot=vm.stack; slot<vm.stackTop; slot++) {
        markValue(*slot);
    }

    for(int i=0; i < vm.frame_count; i++) {
        markObject((Obj*)vm.frames[i].closure);
    }

    for(ObjUpvalue* upvalue = vm.open_upvalues; upvalue != NULL; upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    markTable(&vm.globals);
    markCompilerRoots();
}


static void markArray(ValueArray* array) {
    for(int i=0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

static void blackenObject(Obj* object) {

#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    switch(object->type) {

        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);
            for(int i=0; i < closure->upvalue_count; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
        
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            markObject((Obj*)function->name);
            markArray(&function->chunk.constants);
            break;
        }
        
        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object)->closed);
            break;

        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*)object;
            markObject((Obj*)klass->name);
            break;
        }

        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            markObject((Obj*)instance->klass);
            markTable(&instance->fields);
            break;
        }

        case OBJ_NATIVE:
        case OBJ_STRING:
            break;
    }
}

static void traceReferences() {
    while(vm.gray_count > 0) {
        Obj* object = vm.gray_stack[--vm.gray_count];
        blackenObject(object);
    }
}


static void sweep() {
    Obj* previous = NULL;
    Obj* object = vm.objects;

    while(object != NULL) {
        if(object->is_marked) {
            object->is_marked = false;
            previous = object;
            object = object->next;

        } else {

            Obj* unreached = object;
            object = object->next;

            if(previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}


void collectGarbage() {
    #ifdef DEBUG_LOG_GC
        printf("-- gc begin\n");
    #endif

    size_t before = vm.bytes_allocated;

    markRoots();
    traceReferences();
    tableRemoveWhite(&vm.strings_pool);
    sweep();

    vm.next_GC = vm.bytes_allocated * GC_HEAP_GROW_FACTOR;

    #ifdef DEBUG_LOG_GC
        printf("-- gc end\n");
        printf("   collected %zu bytes (from %zu to %zu) next at %zu\n", 
                before - vm.bytes_allocated, before, vm.bytes_allocated, vm.next_GC);
    #endif
}
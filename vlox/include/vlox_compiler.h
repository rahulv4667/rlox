#ifndef __VLOX_COMPILE_H
#define __VLOX_COMPILE_H

#include "vlox_object.h"
#include "vlox_vm.h"

bool compile(const char* source, Chunk* chunk);

#endif // __VLOX_COMPILE_H
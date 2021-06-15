#ifndef __VLOX_COMPILE_H
#define __VLOX_COMPILE_H

#include "vlox_object.h"
#include "vlox_vm.h"

ObjFunction* compile(const char* source);

#endif // __VLOX_COMPILE_H
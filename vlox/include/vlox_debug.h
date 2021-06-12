#ifndef __VLOX_DEBUG_H
#define __VLOX_DEBUG_H

#include "vlox_common.h"
#include "vlox_chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);
int disassembleInstruction(Chunk* chunk, int offset);

#endif // __VLOX_DEBUG_H
#ifndef __VLOX_CHUNK_H
#define __VLOX_CHUNK_H

#include "vlox_common.h"
#include "vlox_value.h"

typedef enum  {
    OP_CONSTANT,
    OP_RETURN
} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    int* lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
void freeChunk(Chunk* chunk);

int addConstant(Chunk* chunk, Value value);

#endif // __VLOX_CHUNK_H
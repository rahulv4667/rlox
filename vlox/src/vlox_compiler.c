#include <stdio.h>
#include <stdlib.h>

#include "vlox_chunk.h"
#include "vlox_common.h"
#include "vlox_compiler.h"
#include "vlox_scanner.h"
#include "vlox_value.h"
#include "vlox_object.h"

#ifdef VDEBUG_PRINT_CODE
    #include "vlox_debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,          // 
    PREC_ASSIGNMENT,    // =
    PREC_OR,            // or
    PREC_AND,           // and
    PREC_EQUALITY,      // == !=
    PREC_COMPARISON,    // < > <= >=
    PREC_TERM,          // + - 
    PREC_FACTOR,        // * /
    PREC_UNARY,         // ! -
    PREC_CALL,          // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)();

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

Parser parser;
Chunk* compiling_chunk;

static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static void unary();
static void binary();
static void number();
static void string();
static void literal();
static void grouping();




//  token                   = {fn to compile prefix expr starting with token of this type,
//                             fn to compile an infix expr whose left operand is followed by
//                             operand of this type,
//                             precedence of an infix expr that uses this token as an operator};
// int a[] = { [0]=1, [1]=2}; is C99 syntax
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]      = {grouping,    NULL,   PREC_NONE},
    [TOKEN_RIGHT_PAREN]     = {NULL,        NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]      = {NULL,        NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]     = {NULL,        NULL,   PREC_NONE},
    [TOKEN_COMMA]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_DOT]             = {NULL,        NULL,   PREC_NONE},
    [TOKEN_MINUS]           = {unary,       binary, PREC_TERM},
    [TOKEN_PLUS]            = {NULL,        binary, PREC_TERM},
    [TOKEN_SEMICOLON]       = {NULL,        NULL,   PREC_NONE},
    [TOKEN_SLASH]           = {NULL,        binary, PREC_FACTOR},
    [TOKEN_STAR]            = {NULL,        binary, PREC_FACTOR},
    [TOKEN_BANG]            = {unary,       NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]      = {NULL,        binary, PREC_EQUALITY},
    [TOKEN_EQUAL]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]     = {NULL,        binary, PREC_EQUALITY},
    [TOKEN_GREATER]         = {NULL,        binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL]   = {NULL,        binary, PREC_COMPARISON},
    [TOKEN_LESS]            = {NULL,        binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]      = {NULL,        binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]      = {NULL,        NULL,   PREC_NONE},
    [TOKEN_STRING]          = {string,      NULL,   PREC_NONE},
    [TOKEN_NUMBER]          = {number,      NULL,   PREC_NONE},
    [TOKEN_AND]             = {NULL,        NULL,   PREC_NONE},
    [TOKEN_CLASS]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_ELSE]            = {NULL,        NULL,   PREC_NONE},
    [TOKEN_FALSE]           = {literal,     NULL,   PREC_NONE},
    [TOKEN_FOR]             = {NULL,        NULL,   PREC_NONE},
    [TOKEN_FUN]             = {NULL,        NULL,   PREC_NONE},
    [TOKEN_IF]              = {NULL,        NULL,   PREC_NONE},
    [TOKEN_NIL]             = {literal,     NULL,   PREC_NONE},
    [TOKEN_OR]              = {NULL,        NULL,   PREC_NONE},
    [TOKEN_PRINT]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_RETURN]          = {NULL,        NULL,   PREC_NONE},
    [TOKEN_SUPER]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_THIS]            = {NULL,        NULL,   PREC_NONE},
    [TOKEN_TRUE]            = {literal,     NULL,   PREC_NONE},
    [TOKEN_VAR]             = {NULL,        NULL,   PREC_NONE},
    [TOKEN_WHILE]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_ERROR]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_EOF]             = {NULL,        NULL,   PREC_NONE}
};

static Chunk* currentChunk() {
    return compiling_chunk;
}

static void errorAt(Token* token, const char* message) {
    if(parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[Line %d] Error", token->line);

    if(token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if(token->type == TOKEN_ERROR) {

    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
} 

static void advance() {
    parser.previous = parser.current;

    for(;;) {
        parser.current = scanToken();
        if(parser.current.type != TOKEN_ERROR) break;
        errorAtCurrent(parser.current.start); 
    }
}

static void consume(TokenType type, const char* message) {
    if(parser.current.type == type) {
        advance();
        return;
    }
    errorAtCurrent(message);
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if(constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void endCompiler() {
    emitReturn();

#ifdef VDEBUG_PRINT_CODE
    if(!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void number() {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string() {
    // +1 to remove leading quotation
    // -2 is is to remove length calculated for quotation marks
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length-2)));
}

static void grouping() {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary() {
    TokenType operator_type = parser.previous.type;

    // Compile the operand
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch(operator_type) {
        case TOKEN_BANG:    emitByte(OP_NOT);   break;
        case TOKEN_MINUS:   emitByte(OP_NEGATE); break;
        default: return;
    }
}

static void binary() {
    TokenType operator_type = parser.previous.type;
    ParseRule* rule = getRule(operator_type);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operator_type)
    {
        // a>=b can also be written as !(a<b) 
        // This principle is followed.
        case TOKEN_BANG_EQUAL: emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:     emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;

        case TOKEN_PLUS:    emitByte(OP_ADD); break;
        case TOKEN_MINUS:   emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:    emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:   emitByte(OP_DIVIDE); break;
        default: return;    // Unreachable
    }
}


static void literal() {
    switch(parser.previous.type) {
        case TOKEN_FALSE:   emitByte(OP_FALSE); break;
        case TOKEN_TRUE:    emitByte(OP_TRUE); break;
        case TOKEN_NIL:     emitByte(OP_NIL); break;
        default: return;    // Unreachable
    }
}


static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefix_rule = getRule(parser.previous.type)->prefix;
    if(prefix_rule == NULL) {
        error("Expect expression.");
        return;
    }

    prefix_rule();

    while(precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infix_rule = getRule(parser.previous.type)->infix;
        infix_rule();
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

bool compile(const char* source, Chunk* chunk) {
    initScanner(source);

    compiling_chunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();
    expression();
    consume(TOKEN_EOF, "Expect end of expression.");

    endCompiler();
    return !parser.hadError;
}
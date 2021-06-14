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

typedef void (*ParseFn)(bool can_assign);

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

static void unary(bool can_assign);
static void binary(bool can_assign);
static void number(bool can_assign);
static void string(bool can_assign);
static void literal(bool can_assign);
static void grouping(bool can_assign);
static void synchronize();
static void statement();
static void declaration();
static void variable(bool can_assign);
static void namedVariable(Token name, bool can_assign);
static void varDeclaration();
static void printStatement();
static void expressionStatement();
static void defineVariable(uint8_t global);
static uint8_t parseVariable(const char* message);
static uint8_t identifierConstant(Token* identifier_name);


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
    [TOKEN_IDENTIFIER]      = {variable,    NULL,   PREC_NONE},
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

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if(!check(type)) return false;
    advance();
    return true;
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

static void number(bool can_assign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool can_assign) {
    // +1 to remove leading quotation
    // -2 is is to remove length calculated for quotation marks
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length-2)));
}

static void grouping(bool can_assign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary(bool can_assign) {
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

static void binary(bool can_assign) {
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


static void literal(bool can_assign) {
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

    // can_assign is used to avoid expressions like a*b = c/d being evaluated
    // a*(b=c/d). Before assigning, it checks if variable precendence is lower than 
    // assignment
    bool can_assign = precedence <= PREC_ASSIGNMENT;
    prefix_rule(can_assign);

    while(precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infix_rule = getRule(parser.previous.type)->infix;
        infix_rule(can_assign);
    }

    // can assign but assignment is not done. means assignment symbol is used improperly
    if(can_assign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}


// statement      → exprStmt
//                  | printStmt ;
static void statement() {
    if(match(TOKEN_PRINT)) {
        printStatement();
    } else {
        expressionStatement();
    }
}


// declaration    → varDecl
//                  | statement ;
static void declaration() {
    if(match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if(parser.panicMode) synchronize();
}

static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if(match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

static uint8_t parseVariable(const char* error_message) {
    consume(TOKEN_IDENTIFIER, error_message);
    return identifierConstant(&parser.previous);
}

static uint8_t identifierConstant(Token* identifier_name) {
    return makeConstant(OBJ_VAL(copyString(identifier_name->start, identifier_name->length)));
}

static void defineVariable(uint8_t global) {
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static void printStatement() {
    // evaluates and puts the value on stack before OP_PRINT
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void variable(bool can_assign) {
    namedVariable(parser.previous, can_assign);
}

static void namedVariable(Token name, bool can_assign) {
    uint8_t arg = identifierConstant(&name);

    if(can_assign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_GLOBAL, arg);
    } else {
        emitBytes(OP_GET_GLOBAL, arg);
    }
}

static void synchronize() {
    parser.panicMode = false;

    while(parser.current.type != TOKEN_EOF) {

        if(parser.previous.type == TOKEN_SEMICOLON) return;
        switch(parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default: ; //do nothing
        }

        advance();
    }
}

bool compile(const char* source, Chunk* chunk) {
    initScanner(source);

    compiling_chunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();
    // expression();
    // consume(TOKEN_EOF, "Expect end of expression.");
    while(!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}
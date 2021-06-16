#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vlox_chunk.h"
#include "vlox_common.h"
#include "vlox_compiler.h"
#include "vlox_scanner.h"
#include "vlox_value.h"
#include "vlox_object.h"
#include "vlox_memory.h"

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

typedef struct {
    Token name;
    int depth;
    bool is_captured;
} Local;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

typedef struct {
    uint8_t index;
    bool is_local;
} Upvalue;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int local_count;    // number of local variables in the scope.
    Upvalue upvalues[UINT8_COUNT];
    int scope_depth;    // depth of scope.
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool has_superclass;
} ClassCompiler;

Parser parser;
Compiler* current_compiler;
ClassCompiler* current_class = NULL;
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
static void beginScope();
static void endScope();
static void block();
static void variable(bool can_assign);
static void namedVariable(Token name, bool can_assign);
static void varDeclaration();
static void printStatement();
static void whileStatement();
static void returnStatement();
static void ifStatement();
static void forStatement();
static void expressionStatement();
static void classDeclaration();
static void declareVariable();
static void addLocal(Token name);
static int resolveLocal(Compiler* compiler, Token* name);
static bool identifiersEqual(Token* a,Token* b);
static void defineVariable(uint8_t global);
static uint8_t parseVariable(const char* message);
static uint8_t identifierConstant(Token* identifier_name);
static int emitJump(uint8_t instruction);
static void patchJump(int offset);
static void emitLoop(int loop_start);
static void and_(bool can_assign);
static void or_(bool can_assign);
static void function();
static void super_(bool can_assign);
static void funDeclaration();
static void markInitialized();
static void call(bool can_assign);
static uint8_t argumentList();
static int resolveUpvalue(Compiler* compiler, Token* name);
static int addUpvalue(Compiler* compiler, uint8_t index, bool is_local);
static void dot(bool can_assign);
static void this_(bool can_assign);
static void method();

//  token                   = {fn to compile prefix expr starting with token of this type,
//                             fn to compile an infix expr whose left operand is followed by
//                             operand of this type,
//                             precedence of an infix expr that uses this token as an operator};
// int a[] = { [0]=1, [1]=2}; is C99 syntax
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]      = {grouping,    call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]     = {NULL,        NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]      = {NULL,        NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]     = {NULL,        NULL,   PREC_NONE},
    [TOKEN_COMMA]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_DOT]             = {NULL,        dot,    PREC_CALL},
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
    [TOKEN_AND]             = {NULL,        and_,   PREC_AND},
    [TOKEN_CLASS]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_ELSE]            = {NULL,        NULL,   PREC_NONE},
    [TOKEN_FALSE]           = {literal,     NULL,   PREC_NONE},
    [TOKEN_FOR]             = {NULL,        NULL,   PREC_NONE},
    [TOKEN_FUN]             = {NULL,        NULL,   PREC_NONE},
    [TOKEN_IF]              = {NULL,        NULL,   PREC_NONE},
    [TOKEN_NIL]             = {literal,     NULL,   PREC_NONE},
    [TOKEN_OR]              = {NULL,        or_,    PREC_OR},
    [TOKEN_PRINT]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_RETURN]          = {NULL,        NULL,   PREC_NONE},
    [TOKEN_SUPER]           = {super_,      NULL,   PREC_NONE},
    [TOKEN_THIS]            = {this_,       NULL,   PREC_NONE},
    [TOKEN_TRUE]            = {literal,     NULL,   PREC_NONE},
    [TOKEN_VAR]             = {NULL,        NULL,   PREC_NONE},
    [TOKEN_WHILE]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_ERROR]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_EOF]             = {NULL,        NULL,   PREC_NONE}
};

static Chunk* currentChunk() {
    return &current_compiler->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    // printf("========errorAt========\n");
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

#ifdef VDEBUG_PRINT_CODE
    disassembleChunk(currentChunk(), "ERROR");
#endif
}

static void error(const char* message) {
    // printf("========error========\n");

    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    // printf("========errorAtCurrent========\n");

    errorAt(&parser.current, message);
} 

static void advance() {
    // printf("========advance========\n");

    parser.previous = parser.current;

    for(;;) {
        parser.current = scanToken();
        if(parser.current.type != TOKEN_ERROR) break;
        errorAtCurrent(parser.current.start); 
    }
}

static void consume(TokenType type, const char* message) {
    // printf("========consume========\n");

    if(parser.current.type == type) {
        advance();
        return;
    }
    errorAtCurrent(message);
}

static bool check(TokenType type) {
    // printf("========check========\n");

    return parser.current.type == type;
}

static bool match(TokenType type) {
    // printf("========match========\n");

    if(!check(type)) return false;
    advance();
    return true;
}

static uint8_t makeConstant(Value value) {
    // printf("========makeConstant========\n");

    int constant = addConstant(currentChunk(), value);
    if(constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitByte(uint8_t byte) {
    // printf("========emitByte========\n");

    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    // printf("========emitBytes========\n");

    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn() {
    // printf("========emitReturn========\n");

    if(current_compiler->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }
    emitByte(OP_RETURN);
}

static void emitConstant(Value value) {
    // printf("========emitConstant========\n");

    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    // printf("========initCompiler========\n");

    compiler->enclosing = current_compiler;
    compiler->function = NULL;
    compiler->type = type;
    compiler->local_count = 0;
    compiler->scope_depth = 0;
    compiler->function = newFunction(); // should be this way. GC-paranoia.
    current_compiler = compiler;
    if(type != TYPE_SCRIPT) {
        current_compiler->function->name = copyString(parser.previous.start, parser.previous.length);
    }

    // making first element in stack empty. 
    Local* local = &current_compiler->locals[current_compiler->local_count++];
    local->depth = 0;
    local->is_captured = false;
    if(type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 0;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    // printf("========endCompiler========\n");

    emitReturn();
    ObjFunction* function = current_compiler->function;

#ifdef VDEBUG_PRINT_CODE
    if(!parser.hadError) {
        disassembleChunk(currentChunk(), function->name!=NULL?function->name->chars:"<script>");
    }
#endif

    current_compiler = current_compiler->enclosing;
    return function;
}

static void number(bool can_assign) {
    // printf("========number========\n");

    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool can_assign) {
    // printf("========string========\n");

    // +1 to remove leading quotation
    // -2 is is to remove length calculated for quotation marks
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length-2)));
}

static void grouping(bool can_assign) {
    // printf("========grouping========\n");

    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary(bool can_assign) {
    // printf("========unary========\n");

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
    // printf("========binary========\n");

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
    // printf("========literal========\n");

    switch(parser.previous.type) {
        case TOKEN_FALSE:   emitByte(OP_FALSE); break;
        case TOKEN_TRUE:    emitByte(OP_TRUE); break;
        case TOKEN_NIL:     emitByte(OP_NIL); break;
        default: return;    // Unreachable
    }
}


static ParseRule* getRule(TokenType type) {
    // printf("========getRule========\n");

    return &rules[type];
}

static void parsePrecedence(Precedence precedence) {
    // printf("========parsePrecendence========\n");

    advance();
    
    ParseFn prefix_rule = getRule(parser.previous.type)->prefix;
    if(prefix_rule == NULL) {
        printf("%s\n", parser.previous.start);
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
    
    // printf("========expression========\n");

    parsePrecedence(PREC_ASSIGNMENT);
}

// block -> "{" declaration* "}"
static void block() {
    // printf("========block========\n");

    while(!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}


// statement      → exprStmt
//                  | printStmt 
//                  | forStmt
//                  | ifStmt
//                  | whileStmt
//                  | block ;
static void statement() {
    // printf("========statement========\n");

    if(match(TOKEN_PRINT)) {
        printStatement();
    } else if(match(TOKEN_IF)) {
        ifStatement();
    } else if(match(TOKEN_RETURN)) {
        returnStatement();
    } else if(match(TOKEN_FOR)) {
        forStatement();
    } else if(match(TOKEN_WHILE)) {
        whileStatement();
    } else if(match(TOKEN_LEFT_BRACE)) {

        beginScope();
        block();
        endScope();
    
    } else {
        expressionStatement();
    }
}

static void beginScope() {
    // printf("========beginScope========\n");

    current_compiler->scope_depth++;
}

static void endScope() {
    // printf("========endScope========\n");

    current_compiler->scope_depth--;

    // because local variables are on top of stack, they need
    // to be removed when going out of scope.
    while(current_compiler->local_count > 0 && 
            current_compiler->locals[current_compiler->local_count - 1].depth > current_compiler->scope_depth) {

                if(current_compiler->locals[current_compiler->local_count - 1].is_captured) {
                    emitByte(OP_CLOSE_UPVALUE);
                } else {
                    emitByte(OP_POP);
                }
                current_compiler->local_count--;

    } 
}


static void ifStatement() {
    // printf("========ifStatement========\n");

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // currently, stack will have result of condition expression.
    // on seeing OP_JUMP_IF_FALSE, it checks the result and if false,
    // it jumps N bytes depending on parameter passed along with OP_JUMP_IF_FALSE 
    // which is put in chunk by patchJump(). then_jump is index where offset needs to be stored.
    // patchJump(), after parsing then branch, updates offset at this index in bytecode.
    int then_jump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);   // pops condition output from stack if true.
    statement();
    

    int else_jump = emitJump(OP_JUMP);

    patchJump(then_jump);
    emitByte(OP_POP);   // pops condition output from stack if false.

    if(match(TOKEN_ELSE))   statement();
    patchJump(else_jump);
}


static void whileStatement() {
    // printf("========whileStatement========\n");

    int loop_start = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exit_jump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    emitLoop(loop_start);

    patchJump(exit_jump);
    emitByte(OP_POP);
}

static void forStatement() {
    // printf("========forStatement========\n");

    beginScope(); // for any new variables being declared.
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    
    if(match(TOKEN_SEMICOLON)) {
        // No initializer
    } else if(match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loop_start = currentChunk()->count;
    int exit_jump = -1;
    if(!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // jump out of loop if condition is false
        exit_jump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);   // popping condition from stack
    }
    // consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    if(!match(TOKEN_RIGHT_PAREN)) {
        int body_jump = emitJump(OP_JUMP);
        int increment_start = currentChunk()->count;

        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for classes.");

        emitLoop(loop_start);
        loop_start = increment_start;
        patchJump(body_jump);
    }

    statement();
    emitLoop(loop_start);

    if(exit_jump != -1) {
        patchJump(exit_jump); 
        emitByte(OP_POP);   // popping condition.
    }

    endScope();     
}

static void returnStatement() {
    if(current_compiler->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }
    if(match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else if(current_compiler->type == TYPE_INITIALIZER) {
        error("Can't return a value from an initializer.");
    } else {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value");
        emitByte(OP_RETURN);
    }
}

static int emitJump(uint8_t instruction) {
    // printf("========emitJump========\n");

    emitByte(instruction);
    // offset takes two bytes. So, we can jump 2^16 = 65,536 bytes.
    emitByte(0xff);     // offset placeholder
    emitByte(0xff);     // offset placeholder
    return currentChunk()->count - 2;
}

static void emitLoop(int loop_start) {
    // printf("========emitLoop========\n");

    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loop_start + 2;
    if(offset > UINT16_MAX) error("Loop body too large");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static void patchJump(int offset_index) {
    // printf("========patchJump========\n");

    // -2 to adjust for the bytecode for the jump offset itself.
    // if OP_JUMP_IF_FALSE is at 4, then offset will be in 5,6.
    // if 'then branch' ends at 9, then, the value to be store in 
    // offset should 3. we get by 9 - 4 - 2.
    // 9 = current position
    // 4 = byte where JUMP is present.
    // 2 = space being taken by offset which is not part of then code.
    int jump = currentChunk()->count - offset_index - 2;

    if(jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset_index] = (jump>>8) & 0xff;
    currentChunk()->code[offset_index + 1] = jump & 0xff;
}

static void and_(bool can_assign) {
    // printf("========and_========\n");

    int end_jump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(end_jump);
}

static void or_(bool can_assign) {
    // printf("========or_========\n");

    int else_jump = emitJump(OP_JUMP_IF_FALSE);
    int end_jump = emitJump(OP_JUMP);

    patchJump(else_jump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(end_jump);
}

// declaration    → varDecl
//                  | statement ;
static void declaration() {
    // printf("========declaration========\n");
    if(match(TOKEN_CLASS)) {
        classDeclaration();
    } else if(match(TOKEN_FUN)) {
        funDeclaration();
    } else if(match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if(parser.panicMode) synchronize();
}


static void dot(bool can_assign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'");
    uint8_t name = identifierConstant(&parser.previous);

    if(can_assign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if(match(TOKEN_LEFT_PAREN)) {
        uint8_t arg_count = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(arg_count);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token class_name = parser.previous;
    uint8_t name_constant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, name_constant);
    defineVariable(name_constant);

    ClassCompiler class_compiler;
    class_compiler.enclosing = current_class;
    class_compiler.has_superclass = false;
    current_class = &class_compiler;

    if(match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(false);

        if(identifiersEqual(&class_name, &parser.previous)) {
            error("A class can't inherit from itself.");
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);

        namedVariable(class_name, false);
        emitByte(OP_INHERIT);
        class_compiler.has_superclass = true;
    }

    namedVariable(class_name, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");

    while(!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);

    if(class_compiler.has_superclass) {
        endScope();
    }

    current_class = current_class->enclosing;
}

static void super_(bool can_assign) {

    if(current_class == NULL) {
        error("Can't use 'super' outside of a class.");
    } else if(!current_class->has_superclass) {
        error("Can't use 'super' in a class with no superclass.");
    }

    consume(TOKEN_DOT, "Expect '.' after 'super'.");
    consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
    uint8_t name = identifierConstant(&parser.previous);

    namedVariable(syntheticToken("this"), false);
    
    if(match(TOKEN_LEFT_PAREN)) {
        uint8_t arg_count = argumentList();
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_SUPER_INVOKE, name);
        emitByte(arg_count);
    } else {
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_GET_SUPER, name);
    }

}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if(parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }
    function(type);
    emitBytes(OP_METHOD, constant);
}

static void this_(bool can_assign) {
    if(current_class == NULL) {
        error("Can't use 'this' outside of a class.");
    }
    variable(false);
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void call(bool can_assign) {
    uint8_t arg_count = argumentList();
    emitBytes(OP_CALL, arg_count);
}

static uint8_t argumentList() {
    uint8_t arg_count = 0;
    if(!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if(arg_count == 255) {
                error("Can'thave more than 255 arguments.");
            }
            arg_count++;
        }while(match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");

    if(!check(TOKEN_RIGHT_PAREN)) {
        do {
            current_compiler->function->arity++;
            if(current_compiler->function->arity > 255) {
                error("Can't have more than 255 parameters.");
            }

            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);

        }while(match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");

    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for(int i=0; i < function->upvalue_count; i++) {
        emitByte(compiler.upvalues[i].is_local ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void varDeclaration() {
    // printf("========varDeclaration========\n");

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
    // printf("========parseVariable========\n");

    consume(TOKEN_IDENTIFIER, error_message);

    declareVariable();

    // if scope deth is greater than zero, then it is local
    // variable. We dont need to add it to constants
    // in Chunk. We have a separate store for local
    // variables.
    if(current_compiler->scope_depth > 0) return 0;
    return identifierConstant(&parser.previous);
}

static uint8_t identifierConstant(Token* identifier_name) {
    // printf("========identifierConstant========\n");

    return makeConstant(OBJ_VAL(copyString(identifier_name->start, identifier_name->length)));
}

static void addLocal(Token name) {
    // printf("========addLocal========\n");

    if(current_compiler->local_count == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current_compiler->locals[current_compiler->local_count++];
    local->name = name;
    // local->depth = current_compiler->scope_depth;
    // will be set when being initialized[in defineVariable()].
    local->depth = -1;
    local->is_captured = false;
}

static void markInitialized() {
    // printf("========markInitialized========\n");

    if(current_compiler->scope_depth == 0) return;
    current_compiler->locals[current_compiler->local_count - 1].depth = current_compiler->scope_depth;
}


static bool identifiersEqual(Token* a, Token* b) {
    // printf("========identifiersEqual========\n");

    if(a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static void declareVariable() {
    // printf("========declareVariable========\n");

    // return if global scope since it will be taken care of 
    // by identifierConstant() which adds value to constants pool
    if(current_compiler->scope_depth == 0) return;

    Token* name = &parser.previous;

    // detecting redeclaration in same scope.
    for(int i=current_compiler->local_count - 1; i>=0; i--) {
        Local* local = &current_compiler->locals[i];

        // if not finding variable in current scope, we can
        // exit as it is not redeclaration.
        if(local->depth != -1 && local->depth < current_compiler->scope_depth) {
            break;
        }

        // checking if variable name matches with existing ones
        // in current scope.
        if(identifiersEqual(name, &local->name)) {
            error("Already variable with this name in this scope.");
        }
    }

    // method where compiler records the existence of 
    // local variable.
    addLocal(*name);
}

static void defineVariable(uint8_t global) {
    // printf("========defineVariable========\n");

    // if in local scope, then it wont be added coz
    // it is not present in constants pool. so, the 'global'
    // value is almost useless. The local value is right
    // on top of the stack since it is just declared.
    if(current_compiler->scope_depth > 0)
        return;
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static void printStatement() {
    // printf("========printStatement========\n");

    // evaluates and puts the value on stack before OP_PRINT
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void expressionStatement() {
    // printf("========expressionStatement========\n");

    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void variable(bool can_assign) {
    // printf("========variable========\n");

    namedVariable(parser.previous, can_assign);
}

static void namedVariable(Token name, bool can_assign) {
    // printf("========namedVariable========\n");

    uint8_t get_op, set_op;

    // check if local scope and if variable is in local scope.
    int arg = resolveLocal(current_compiler, &name);
    if(arg != -1) {
        get_op = OP_GET_LOCAL;
        set_op = OP_SET_LOCAL;
    } else if((arg = resolveUpvalue(current_compiler, &name)) != -1) {
        get_op = OP_GET_UPVALUE;
        set_op = OP_SET_UPVALUE;
    } else {

        // it is a global varibale. Put it in constant pool.
        arg = identifierConstant(&name);
        get_op = OP_GET_GLOBAL;
        set_op = OP_SET_GLOBAL;
    }

    // if being assigned something, set it.
    // else, get it.
    if(can_assign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(set_op, (uint8_t)arg);
    } else {
        emitBytes(get_op, (uint8_t)arg);
    }
}

int resolveLocal(Compiler* compiler, Token* name) {
    // printf("========resolveLocal========\n");


    for(int i=compiler->local_count-1; i>=0; i--) {
        Local* local = &compiler->locals[i];
        if(identifiersEqual(name, &local->name)) {

            // handles cases like 'var a = a;'
            if(local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }

            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool is_local) {
    int upvalue_count = compiler->function->upvalue_count;

    for(int i=0; i<upvalue_count; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if(upvalue->index == index && upvalue->is_local == is_local) {
            return i;
        } 
    }

    if(upvalue_count == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalue_count].is_local = is_local;
    compiler->upvalues[upvalue_count].index = index;
    return compiler->function->upvalue_count++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    if(compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if(local != -1) {
        compiler->enclosing->locals[local].is_captured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if(upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

static void synchronize() {
    // printf("========synchronize========\n");

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

ObjFunction* compile(const char* source) {
    // printf("========compile========\n");

    initScanner(source);

    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    // compiling_chunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();
    // expression();
    // consume(TOKEN_EOF, "Expect end of expression.");
    while(!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError?NULL:function;
}

void markCompilerRoots() {
    Compiler* compiler = current_compiler;
    while(compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
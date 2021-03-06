package rlox.Parser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import rlox.Lox;
import rlox.Scanner.*;

//////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// LANGUAGE GRAMMAR //////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//                                                                                      //
//    program        → declaration* EOF ;                                               //
//    declaration    → classDecl | funDecl | varDecl | statement ;                      //
//    varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;                       //
//    funDecl        → "fun" function ;                                                 //
//    classDecl      → "class" IDENTIFIER ("<" IDENTIFIER)? "{" function* "}" ;         //
//    function       → IDENTIFIER "(" parameters? ")" block ;                           //
//    parameters     → IDENTIFIER ( "," IDENTIFIER )* ;                                 //
//    statement      → exprStmt | ifStmt | whileStmt | returnStmt                       //
//                      | printStmt | block;                                            //
//    block          → "{" declaration* "}" ;                                           //
//    exprStmt       → expression ";" ;                                                 //
//    printStmt      → "print" expression ";" ;                                         //
//    ifStmt         → "if" "(" expression ")" statement                                //
//                     ( "else" statement )? ;                                          //
//    whileStmt      → "while" "(" expression ")" statement ;                           //
//    forStmt        → "for" "(" ( varDecl | exprStmt | ";" )                           //
//                      expression? ";"                                                 //
//                      expression? ")" statement ;                                     //
//    returnStmt     → "return" expression? ";" ;                                       //
//    expression     → assignment ;                                                     //
//    assignment     → ( call "." )? IDENTIFIER "=" assignment                          //
//                   | logic_or ;                                                       //
//    logic_or       → logic_and ( "or" logic_and )* ;                                  //
//    logic_and      → equality ( "and" equality )* ;                                   //
//    equality       → comparison ( ( "!=" | "==" ) comparison )* ;                     //
//    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;                     //
//    term           → factor ( ( "-" | "+" ) factor )* ;                               //
//    factor         → unary ( ( "/" | "*" ) unary )* ;                                 //
//    unary          → ( "!" | "-" ) unary | call ;                                     //
//    call           → primary ( "(" arguments? ")" )*                                  //
//                     | "."IDENTIFIER;                                                 //
//    arguments      → expression ( "," expression )* ;                                 //
//    primary        → NUMBER | STRING | "true" | "false" | "nil"                       //
//                     | "(" expression ")" | IDENTIFIER | "super" "." IDENTIFIER ;     //
//                                                                                      //
//////////////////////////////////////////////////////////////////////////////////////////

public class Parser {
    
    private static class ParseError extends RuntimeException {}
    
    private final List<Token> tokens;
    private int current = 0;


    public Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    public List<Stmt> parse() {
        List<Stmt> statements = new ArrayList<>();
        while(!isAtEnd()) {
            // statements.add(statement());
            statements.add(declaration());
        }

        return statements;
    }


    private boolean match(TokenType... types) {
        for(TokenType type: types) {
            if(check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    private boolean check(TokenType type) {
        if(isAtEnd()) return false;
        return peek().type == type;
    }

    private Token advance() {
        if(!isAtEnd()) current++;
        return previous();
    }

    private boolean isAtEnd() {
        return peek().type == TokenType.EOF;
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token previous() {
        return tokens.get(current - 1);
    }


    //    declaration    → funDecl | varDecl | statement ;
    private Stmt declaration() {
        try {
            // System.out.println("declaration    → funDecl | varDecl | statement ;");

            if(match(TokenType.CLASS))
                return classDeclaration();
            if(match(TokenType.FUN))
                return function("function");

            if(match(TokenType.VAR)) 
                return varDeclaration();

            return statement();
        } catch(ParseError error) {
            synchronize();
            return null;
        }
    }


    // classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
    //                    "{" function* "}" ;
    private Stmt classDeclaration() {
        // System.out.println("classDecl → 'class' IDENTIFIER ( '<' IDENTIFIER )?'{' function* '}' ;");
        
        Token name = consume(TokenType.IDENTIFIER, "Expect class name.");
        
        Expr.Variable superclass = null;
        if(match(TokenType.LESS)) {
            consume(TokenType.IDENTIFIER, "Expect superclass name.");
            superclass = new Expr.Variable(previous());
        }
        
        consume(TokenType.LEFT_BRACE, "Expect '{' before class body.");

        List<Stmt.Function> methods = new ArrayList<>();
        while(!check(TokenType.RIGHT_BRACE) && !isAtEnd()) {
            methods.add(function("method"));
        }

        consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.");
        return new Stmt.Class(name, superclass, methods);
    }


    //    varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    private Stmt varDeclaration() {
        // System.out.println("varDecl        → 'var' IDENTIFIER ( '=' expression )? ';' ;");

        Token name = consume(TokenType.IDENTIFIER, "Expect variable name.");

        Expr initializer = null;
        if(match(TokenType.EQUAL)) {
            initializer = expression();
        }

        consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");
        return new Stmt.Var(name, initializer);
    }


    //    funDecl        → "fun" function ;
    //    function       → IDENTIFIER "(" parameters? ")" block ;
    private Stmt.Function function(String kind) {
        // System.out.println(" function       → IDENTIFIER "(" parameters? ")" block ;");

        Token name = consume(TokenType.IDENTIFIER, "Expect "+kind+" name.");
        consume(TokenType.LEFT_PAREN, "Expect '(' after "+kind+" name.");

        List<Token> parameters = new ArrayList<>();
        if(!check(TokenType.RIGHT_PAREN)) {
            do {
                if(parameters.size() >= 255) {
                    error(peek(), "Can't have more than 255 parameters.");
                }
                parameters.add(consume(TokenType.IDENTIFIER, 
                                "Expect parameter name."));
            } while(match(TokenType.COMMA));
        }

        consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");

        consume(TokenType.LEFT_BRACE, "Expect '{' before "+kind+" body.");
        List<Stmt> body = block();
        return new Stmt.Function(name, parameters, body);
    }

    //    parameters     → IDENTIFIER ( "," IDENTIFIER )* ;

    //    statement      → exprStmt | ifStmt | whileStmt | forStmt | returnStmt
    //                      | printStmt | block ; 
    private Stmt statement() {
        // System.out.println("statement → exprStmt|returnStmt|ifStmt|whileStmt|forStmt|printStmt|block;");

        if(match(TokenType.IF)) return ifStatement();
        if(match(TokenType.WHILE)) return whileStatement();
        if(match(TokenType.FOR)) return forStatement();
        if(match(TokenType.PRINT)) return printStatement();
        if(match(TokenType.RETURN)) return returnStatement();
        if(match(TokenType.LEFT_BRACE)) return new Stmt.Block(block());
        return expressionStatement();
    }

    // ifStmt         → "if" "(" expression ")" statement
    //                  ( "else" statement )? ;
    private Stmt ifStatement() {
        // System.out.println("ifStmt → 'if' '(' expression ')' statement( 'else' statement )? ;");
        
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
        Expr condition = expression();
        consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

        Stmt thenBranch = statement();
        Stmt elseBranch = null;
        if(match(TokenType.ELSE)) {
            elseBranch = statement();
        }
        return new Stmt.If(condition, thenBranch, elseBranch);
    }

    // whileStmt      → "while" "(" expression ")" statement ;
    private Stmt whileStatement() {
        // System.out.println("whileStmt      → 'while' '(' expression ')' statement ;");
        
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
        Expr condition = expression();
        consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

        Stmt body = statement();
        return new Stmt.While(condition, body);
    }

    // forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
    //                  expression? ";"
    //                  expression? ")" statement ;
    private Stmt forStatement() {

        // For is implemented in terms of while.
        // So, for statements are changed to fit while execution
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");

        Stmt initializer;
        if(match(TokenType.SEMICOLON)) {
            initializer = null;
        } else if(match(TokenType.VAR)) {
            initializer = varDeclaration();
        } else {
            initializer = expressionStatement();
        }

        Expr condition = null;
        if(!check(TokenType.SEMICOLON)) {
            condition = expression();
        }

        consume(TokenType.SEMICOLON, "Expect ';' after loop condition.");

        Expr increment = null;
        if(!check(TokenType.RIGHT_PAREN)) {
            increment = expression();
        }
        consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.");

        Stmt body = statement();

        // creating a 'while' body by combining actual 'for' body and increment statement.
        if(increment != null) {
            body = new Stmt.Block(Arrays.asList(body, new Stmt.Expression(increment)));
        }

        // if no condition is given, it means infinite loop.
        // so, true is passed to while loop.
        if(condition == null) 
            condition =  new Expr.Literal(true);
        body = new Stmt.While(condition, body);

        // We have the while body and condition. We need to combine it with initiizer
        if(initializer != null) {
            body = new Stmt.Block(Arrays.asList(initializer, body));
        }

        return body;
    }


    // returnStmt     → "return" expression? ";" ;
    private Stmt returnStatement() {
        Token keyword = previous();
        Expr value = null;
        if(!check(TokenType.SEMICOLON)) {
            value = expression();
        }

        consume(TokenType.SEMICOLON, "Expect ';' after return value.");
        return new Stmt.Return(keyword, value);
    }


    //    block          → "{" declaration* "}" ; 
    private List<Stmt> block() {
        // System.out.println("block  → '{' declaration* '}' ;");

        List<Stmt> statements = new ArrayList<>();

        while(!check(TokenType.RIGHT_BRACE) && !isAtEnd()) {
            statements.add(declaration());
        }

        consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    //    printStmt      → "print" expression ";" ;
    private Stmt printStatement() {
        // System.out.println("printStmt      → 'print' expression ';' ;");

        Expr value = expression();
        consume(TokenType.SEMICOLON, "Expect ; after value.");
        return new Stmt.Print(value);
    }

    //    exprStmt       → expression ";" ;
    private Stmt expressionStatement() {
        // System.out.println("exprStmt       → expression ';' ;");
        Expr expr = expression();
        consume(TokenType.SEMICOLON, "Expect ; after expression.");
        return new Stmt.Expression(expr);
    }

    //  expression     → assignment ;    
    private Expr expression() {
        // System.out.println("expression     → assignment");
        return assignment();
    }

    // assignment     → ( call "." )? IDENTIFIER "=" assignment
    //                | logic_or ;
    private Expr assignment() {
        // System.out.println("assignment     → IDENTIFIER '=' assignment | logic_or");

        Expr expr = or();

        if(match(TokenType.EQUAL)) {
            Token equals = previous();
            Expr value = assignment();

            if(expr instanceof Expr.Variable) {
                Token name = ((Expr.Variable)expr).name;
                return new Expr.Assign(name, value);
            } else if(expr instanceof Expr.Get) {
                Expr.Get get = (Expr.Get)expr;
                return new Expr.Set(get.object, get.name, value);
            }

            error(equals, "Invalid assignment target.");
        }

        return expr;
    }

    //    logic_or       → logic_and ( "or" logic_and )* ; 
    private Expr or() {
        // System.out.println("logic_or       → logic_and ( 'or' logic_and )* ;");

        Expr expr = and();

        while(match(TokenType.OR)) {
            Token operator = previous();
            Expr right = and();
            expr = new Expr.Logical(expr, operator, right);
        }

        return expr;
    }

    //    logic_and      → equality ( "and" equality )* ;    
    private Expr and() {
        // System.out.println("logic_and      → equality ( 'and' equality )* ; ");

        Expr expr = equality();

        while(match(TokenType.AND)) {
            Token operator = previous();
            Expr right = equality();
            expr = new Expr.Logical(expr, operator, right);
        }

        return expr;
    }

    //  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    private Expr equality() {
        // System.out.println("equality       → comparison ( ( '!=' | '==' ) comparison )*");
        Expr expr = comparison();

        while(match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
            Token operator = previous();
            Expr right = comparison();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }


    //    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;    //  
    private Expr comparison() {
        // System.out.println("comparison     → term ( ( '>' | '>=' | '<' | '<=' ) term )* ");
        Expr expr = term();

        while(match(TokenType.GREATER, TokenType.GREATER_EQUAL, 
                    TokenType.LESS, TokenType.LESS_EQUAL)) {

                        Token operator = previous();
                        Expr right = term();
                        expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    //    term           → factor ( ( "-" | "+" ) factor )* ;
    private Expr term() {
        // System.out.println("term           → factor ( ( '-' | '+' ) factor )*");
        Expr expr = factor();

        while(match(TokenType.MINUS, TokenType.PLUS)) {
            
            Token operator = previous();
            Expr right = factor();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    //    factor         → unary ( ( "/" | "*" ) unary )* ;                //
    private Expr factor() {
        // System.out.println("factor         → unary ( ( '/' | '*' ) unary )*");
        Expr expr = unary();

        while(match(TokenType.SLASH, TokenType.STAR)) {

            Token operator = previous();
            Expr right = unary();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }


    //    unary          → ( "!" | "-" ) unary | call; 
    private Expr unary() {
        // System.out.println("unary          → ( '!' | '-' ) unary | call");
        if(match(TokenType.BANG, TokenType.MINUS)) {
            Token operator = previous();
            Expr right = unary();
            return new Expr.Unary( operator, right);
        }

        return call();
    }

    // call  → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    private Expr call() {
        Expr expr = primary();

        while(true) {
            if(match(TokenType.LEFT_PAREN)) {
                expr = finishCall(expr);
            } else if(match(TokenType.DOT)) {
                Token name = consume(TokenType.IDENTIFIER, 
                            "Expect property name after '.'");
                expr = new Expr.Get(expr, name);
            } else {
                break;
            }
        }

        return expr;
    }

    // arguments      → expression ( "," expression )* ;
    private Expr finishCall(Expr callee) {
        List<Expr> arguments = new ArrayList<>();

        if(!check(TokenType.RIGHT_PAREN)) {
            do {
                // Having a limit on number of arguments that can be passed
                // to a function makes things simple when having bytecode interpreter
                if(arguments.size() >= 255) {
                    error(peek(), "Can't have more than 255 arguments.");
                }
                arguments.add(expression());
            }while(match(TokenType.COMMA));
        }

        Token paren = consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");

        return new Expr.Call(callee, paren, arguments);
    }
    

    //    primary        → NUMBER | STRING | "true" | "false" | "nil"      //
    //                     | "(" expression ")"  | "super" "." IDENTIFIER;
    private Expr primary() {
        // System.out.println("primary  → NUMBER|STRING|'true'|'false' | 'nil'|'(' expression ')'");
        if(match(TokenType.FALSE)) return new Expr.Literal(false);
        if(match(TokenType.TRUE)) return new Expr.Literal(true);
        if(match(TokenType.NIL)) return new Expr.Literal(null);

        if(match(TokenType.NUMBER, TokenType.STRING)) {
            return new Expr.Literal(previous().literal);
        }

        if(match(TokenType.SUPER)) {
            Token keyword = previous();
            consume(TokenType.DOT, "Expect '.' after 'super'.");

            Token method = consume(TokenType.IDENTIFIER, "Expect superclass method name");
            return new Expr.Super(keyword, method);
        }

        if(match(TokenType.THIS)) {
            return new Expr.This(previous());
        }

        if(match(TokenType.IDENTIFIER)) {
            return new Expr.Variable(previous());
        }

        if(match(TokenType.LEFT_PAREN)) {
            Expr expr = expression();
            consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
            return new Expr.Grouping(expr);
        }
        
        throw error(peek(), "Expect expression.");
    }

    private Token consume(TokenType type, String message) {
        if(check(type)) return advance();
        throw error(peek(), message);
    }

    private ParseError error(Token token, String message) {
        Lox.error(token, message);
        return new ParseError();
    }

    private void synchronize() {
        advance();

        while(!isAtEnd()) {
            if(previous().type == TokenType.SEMICOLON) return;

            switch(peek().type) {
                case CLASS:
                case FUN:
                case VAR:
                case FOR:
                case IF:
                case WHILE:
                case PRINT:
                case RETURN:
                    return;
            }

            advance();
        }
    }
}

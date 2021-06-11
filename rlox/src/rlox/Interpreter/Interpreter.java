package rlox.Interpreter;

import rlox.Scanner.*;
import rlox.Parser.*;
import rlox.Parser.Expr.Binary;
import rlox.Parser.Expr.Grouping;
import rlox.Parser.Expr.Literal;
import rlox.Parser.Expr.Unary;
import rlox.Parser.Expr.Variable;
import rlox.Parser.Stmt.Expression;
import rlox.Parser.Stmt.Print;
import rlox.Parser.Stmt.Var;

import java.util.List;

import rlox.Lox;

public class Interpreter implements 
                        Expr.Visitor<Object>,
                        Stmt.Visitor<Void> {

    private Environment environment = new Environment();

    // public void interpret(Expr expression) {
    //     try {
    //         Object value = evaluate(expression);
    //         System.out.println(stringify(value));
    //     } catch(RuntimeError error) {
    //         Lox.runtimeError(error);
    //     }
    // }
    public void interpret(List<Stmt> statements) {
        try {
            for(Stmt statement: statements) {
                execute(statement);
            }
        } catch(RuntimeError error) {
            Lox.runtimeError(error);
        }
    }

    private void execute(Stmt stmt) {
        stmt.accept(this);
    }

    private void executeBlock(List<Stmt> statements, Environment environment) {
        Environment previous = this.environment;

        try {
            this.environment = environment;

            for(Stmt statement: statements) {
                execute(statement);
            }
        } finally {
            this.environment = previous;
        }
    }


    @Override
    public Object visitBinaryExpr(Binary expr) {
        Object left = evaluate(expr.left);
        Object right = evaluate(expr.right);

        switch(expr.operator.type) {
            case GREATER:
                checkNumberOperands(expr.operator, left, right);
                return (double)left  > (double)right;
            case GREATER_EQUAL:
                checkNumberOperands(expr.operator, left, right);
                return (double)left >= (double)right;
            case LESS:
                checkNumberOperands(expr.operator, left, right);
                return (double)left  < (double)right;
            case LESS_EQUAL:
                checkNumberOperands(expr.operator, left, right);
                return (double)left <= (double)right;
            case BANG_EQUAL: return !isEqual(left, right);
            case EQUAL_EQUAL: return isEqual(left, right);
            case MINUS:
                checkNumberOperands(expr.operator, left, right);
                return (double)left  -  (double)right;
            case SLASH:
                checkNumberOperands(expr.operator, left, right);
                return (double)left  /  (double)right;
            case STAR:
                checkNumberOperands(expr.operator, left, right);
                return (double)left  *  (double)right;

            case PLUS:
                if(left instanceof Double && right instanceof Double) {
                    return (double)left  +  (double)right;
                }
                if(left instanceof String && right instanceof String) {
                    return (String)left + (String)right;
                }
                
                throw new RuntimeError(expr.operator, 
                                    "Operands must be two numbers of two strings.");
        }
        return null;
    }

    @Override
    public Object visitGroupingExpr(Grouping expr) {
        return evaluate(expr.expression);
    }

    @Override
    public Object visitLiteralExpr(Literal expr) {
        return expr.value;
    }

    @Override
    public Object visitUnaryExpr(Unary expr) {
        Object right = expr.right;

        switch(expr.operator.type) {
            case BANG:
                return !isTruthy(right);
            case MINUS:
                checkNumberOperand(expr.operator, right);
                return -(double)right;
        }
        return null;
    }

    @Override
    public Void visitExpressionStmt(Expression stmt) {
        evaluate(stmt.expression);
        return null;
    }

    @Override
    public Void visitPrintStmt(Print stmt) {
        Object value = evaluate(stmt.expression);
        System.out.println(stringify(value));
        return null;
    }

    @Override
    public Void visitVarStmt(Var stmt) {
        Object value = null;
        if(stmt.initializer != null) {
            value = evaluate(stmt.initializer);
        }

        environment.define(stmt.name.lexeme, value);
        return null;
    }

    @Override
    public Object visitVariableExpr(Variable expr) {
        return environment.get(expr.name);
    }

    @Override
    public Object visitAssignExpr(Expr.Assign expr) {
        Object value = evaluate(expr.value);
        environment.assign(expr.name, value);
        return value;
    }

    @Override
    public Void visitBlockStmt(Stmt.Block stmt) {
        executeBlock(stmt.statements, new Environment(environment));
        return null;
    }

    @Override
    public Void visitIfStmt(Stmt.If stmt) {
        if(isTruthy(evaluate(stmt.condition))) {
            execute(stmt.thenBranch);
        } else if(stmt.elseBranch != null) {
            execute(stmt.elseBranch);
        }
        return null;
    }

    @Override
    public Object visitLogicalExpr(Expr.Logical expr) {
        Object left = evaluate(expr.left);

        if(expr.operator.type == TokenType.OR) {
            if(isTruthy(left)) return left;
        } else {
            if(!isTruthy(left)) return left;
        }

        return evaluate(expr.right);
    }

    @Override
    public Void visitWhileStmt(Stmt.While stmt) {
        while(isTruthy(evaluate(stmt.condition))) {
            execute(stmt.body);
        }
        return null;
    }

    private void checkNumberOperand(Token operator, Object operand) {
        if(operand instanceof Double) return;
        throw new RuntimeError(operator, "Operand must be a number.");
    }

    private void checkNumberOperands(Token operator, Object left_oprnd, Object right_oprnd) {
        if(left_oprnd instanceof Double && right_oprnd instanceof Double) 
            return;
        throw new RuntimeError(operator, "Operand must be a number.");
    }
    
    private Object evaluate(Expr expr) {
        return expr.accept(this);
    }

    private boolean isTruthy(Object object) {
        // Lox follows Ruby's truthy principles.
        // false and nil are falsy. Everything else is truthy.
        if(object == null) return false;
        if(object instanceof Boolean) return (boolean)object; 
        return true;
    }

    private boolean isEqual(Object left, Object right) {
        if(left == null && right == null) return true;
        if(left == null) return false;
        return left.equals(right);
    }

    private String stringify(Object value) {
        if(value == null) return "nil";

        if(value instanceof Double) {
            String text = value.toString();
            if(text.endsWith(".0")) {
                text = text.substring(0, text.length()-2);
            }
            return text;
        }

        return value.toString();
    }
    
}
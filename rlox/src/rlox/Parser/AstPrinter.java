// package rlox.Parser;

// import rlox.Parser.Expr.Binary;
// import rlox.Parser.Expr.Grouping;
// import rlox.Parser.Expr.Literal;
// import rlox.Parser.Expr.Unary;

// public class AstPrinter implements Expr.Visitor<String> {

//     public String print(Expr expr) {
//         return expr.accept(this);
//     }

//     private String parenthesize(String name, Expr ...exprs) {
//         StringBuilder builder = new StringBuilder();

//         builder.append("(").append(name);
//         for(Expr expr: exprs) {
//             builder.append(" ");
//             builder.append(expr.accept(this));
//         }
//         builder.append(")");

//         return builder.toString();
//     }

//     @Override
//     public String visitBinaryExpr(Binary expr) {
//         return parenthesize(expr.operator.lexeme, expr.left, expr.right);
//     }

//     @Override
//     public String visitGroupingExpr(Grouping expr) {
//         return parenthesize("group", expr.expression);
//     }

//     @Override
//     public String visitLiteralExpr(Literal expr) {
//         if(expr.value == null) return "nil";
//         return expr.value.toString();
//     }

//     @Override
//     public String visitUnaryExpr(Unary expr) {
//         return parenthesize(expr.operator.lexeme, expr.right);
//     }

    
// }
package rlox;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;


import rlox.Scanner.*;
import rlox.Interpreter.*;
import rlox.Parser.*;

public class Lox {
    static boolean hadError = false;
    static boolean hadRuntimeError = false;
    private static final Interpreter interpreter = new Interpreter();

    private static void runFile(String path) throws IOException {
        byte[] bytes = Files.readAllBytes(Paths.get(path));
        run(new String(bytes, Charset.defaultCharset()));

        // indicaate an error in the exit code
        if(hadError) System.exit(65);
        if(hadRuntimeError) System.exit(65);
    }

    private static void runPrompt() throws IOException {
        InputStreamReader input = new InputStreamReader(System.in);
        BufferedReader reader = new BufferedReader(input);

        while(true) {
            System.out.print("###\t");
            String line = reader.readLine();
            if (line == null) break;
            run(line);

            hadError = false;
        }
    }

    private static void run(String source) {
        Scanner scanner = new Scanner(source);
        List<Token> tokens = scanner.scanTokens();
        // for(Token token: tokens) {
        //     System.out.println(token.toString());
        // }
        Parser parser = new Parser(tokens);
        // Expr expression = parser.parse();
        List<Stmt> statements = parser.parse();

        // Stop if there was a syntax error
        if(hadError) return;

        // Resolving variables
        Resolver resolver = new Resolver(interpreter);
        resolver.resolve(statements);
        
        // Stop if there was a resolution error
        if(hadError) return;

        interpreter.interpret(statements);

        // System.out.println(new AstPrinter().print(expression));
    }

    static public void error(int line, String message) {
        report(line, "", message);
    }

    static public void error(Token token, String message) {
        if(token.type == TokenType.EOF) {
            report(token.line, " at end", message);
        } else {
            report(token.line, " at '" + token.lexeme + "' ", message);
        }
    }

    private static void report(int line, String where, String message) {
        System.err.println("[ Line "+line+" ] Error "+where+" : "+message);
        hadError = true;
    }

    static public void runtimeError(RuntimeError error) {
        System.out.println(error.getMessage()+"\n[ Line "+error.token.line+" ]");
        hadRuntimeError = true;
    }

    public static void main(String[] args) throws IOException {

        if(args.length > 1) {
            System.out.println("Usage: rlox [script]");
            System.exit(64);
        } else if (args.length == 1) {
            runFile(args[0]);
        } else {
            runPrompt();
        }

    }
}

package rlox.Interpreter;

import java.util.HashMap;
import java.util.Map;

import rlox.Scanner.Token;

public class Environment {

    final Environment enclosing;
    private final Map<String, Object> values = new HashMap<>(); 

    Environment() {
        enclosing = null;
    }

    Environment(Environment enclosing) {
        this.enclosing = enclosing;
    }
    
    public void define(String name, Object value) {
        values.put(name, value);
    }

    public Object get(Token name) {
        if(values.containsKey(name.lexeme)) {
            return values.get(name.lexeme);
        }

        if(enclosing != null)
            return enclosing.get(name);

        throw new RuntimeError(name, "Undefined variable '"+ name.lexeme + "'.");
    }

    public void assign(Token name, Object value) {
        if(values.containsKey(name.lexeme)) {
            values.put(name.lexeme, value);
            return;
        }

        if(enclosing != null) {
            enclosing.assign(name, value);
            return;
        }

        throw new RuntimeError(name, "Undefined variable '"+ name.lexeme + "'.");
    }

    public Object getAt(int distance, String name) {
        return ancestor(distance).values.get(name);
    }

    public void assignAt(int distance, Token name, Object value) {
        ancestor(distance).values.put(name.lexeme, value);
    }

    Environment ancestor(int distance) {
        Environment environment = this;
        for(int i=0; i<distance; i++) {
            environment = environment.enclosing;
        }

        return environment;
    }
}

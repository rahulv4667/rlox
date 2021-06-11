package rlox.Interpreter;

public class Return extends RuntimeException {
    final Object value;

    Return(Object value) {
        // the following super will stop JVM to do extra stuff like 
        // stack trace and error handling
        super(null, null, false, false);
        this.value = value;
    }
}

package myapp;

import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.Stack;

class ParserException extends Exception {
    public ParserException(String message) {
        super(message);
    }
}

public class Parser {
    public Parser() {}

    private Operation toOperation(char c) {
        return switch (c) {
            case '+' -> Operation.ADD;
            case '-' -> Operation.SUB;
            case '*' -> Operation.MUL;
            case '/' -> Operation.DIV;
            case '^' -> Operation.POW;
            default -> null;
        };
    }

    public ParserData parse(String input) throws ParserException {
        double n1 = 0, n2 = 0;
        Operation op = Operation.NULL;


        // Simply look for (num) ops (num) pattern
        StreamTokenizer tokenizer = new StreamTokenizer(new StringReader(input));
        tokenizer.ordinaryChar('-');
        tokenizer.ordinaryChar('/');
        tokenizer.ordinaryChar('*');
        tokenizer.ordinaryChar('+');
        tokenizer.ordinaryChar('^');
        try{
            // Simply look for [number] [op] [number]
            if (tokenizer.nextToken() != StreamTokenizer.TT_NUMBER){
                throw new ParserException("Expected number");
            }
            n1 =  tokenizer.nval;

            if  (tokenizer.nextToken() == StreamTokenizer.TT_EOF) {
                throw new ParserException("Uncompleted operation");
            }

            op = toOperation((char) tokenizer.ttype);
            if (op == null) {
                throw new ParserException("Operation: " + op + " not supported");
            }

            if (tokenizer.nextToken() != StreamTokenizer.TT_NUMBER){
                throw new ParserException("Expected number");
            }
            n2 =  tokenizer.nval;

            if (tokenizer.nextToken() != StreamTokenizer.TT_EOF) {
                throw new ParserException("Unexpected token(s) after second number");
            }

        } catch(java.io.IOException e){
            System.err.println(e);
        }


        Stack<Operation> stack = new Stack<>();
        stack.push(op);

        Stack<Double> nums = new Stack<>();
        nums.push(n2);
        nums.push(n1);
        return new ParserData(stack, nums);
    }
}

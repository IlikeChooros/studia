package myapp;

import java.util.Stack;

class ParserData {
    public ParserData(Stack<Operation> operations, Stack<Double> data) {
        this.data = data;
        this.stack = operations;
    }
    public Stack<Operation> stack = new Stack<>();
    public Stack<Double> data = new Stack<Double>();
}

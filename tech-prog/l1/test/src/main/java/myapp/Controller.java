package myapp;

import java.util.Scanner;

public class Controller {
    private Model model;
    private Parser parser;

    public Controller(Model model) {
        this.model = model;
        this.parser = new Parser();
    }

    private String apply(ParserData data) {
        // Apply every instruction on the stack
        double n1, n2;
        Operation op;

        while(!data.stack.empty() && !data.data.empty()) {
            n1 = data.data.pop();
            op = data.stack.pop();
            n2 = data.data.pop();

            model.set(n1, n2);
            double result = switch (op) {
                case ADD -> model.add();
                case SUB -> model.sub();
                case MUL -> model.mul();
                case DIV -> model.div();
                case POW -> model.pow();
                default -> 0;
            };

            data.data.push(result);
        }

        return data.data.pop().toString();
    }

    public void run() {

        System.out.println("Jcalc");
        Scanner sc = new Scanner(System.in);
        while (true) {
            System.out.print("> ");

            // Read the input for the calculator from the user
            String input = sc.nextLine();

            String result = "";
            try {
                result = apply(parser.parse(input));
            } catch (ParserException e) {
                result = ("Error: " + e.getMessage());
            }

            if (input.equals("exit") || input.equals("quit")) {
                break;
            }

            System.out.println(result);
        }
        sc.close();
    }


}

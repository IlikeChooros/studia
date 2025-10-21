package myapp;

/**
 * Calculator app
 */
public class App {
    public static void main(String[] args) {
        Controller controller = new Controller(
                new Calculator()
        );

        controller.run();
    }
}

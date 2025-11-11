package app;


public final class App {

    private App() {
        // Prevent instantiation
    }

    /**
     * @param args the command line arguments
     */
    public static void main(final String[] args) {
        Controller controller = new Controller();
        controller.run();
    }
}

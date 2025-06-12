import java.io.PrintWriter;
import java.util.Stack;

public class ServerMessenger {
    public static final String BEGIN_TOKEN = "<!BEGIN!>";
    public static final String END_TOKEN = "<!END!>";

    private final PrintWriter out;
    private final Stack<String> messages = new Stack<>();

    public ServerMessenger(PrintWriter writer) {
        out = writer;
    }

    /**
     * Begin the transaction
     */
    void begin() {
        messages.clear();
        messages.add(BEGIN_TOKEN);
    }

    /**
     * Appends given message to the stack
     */
    void println(String message) {
        messages.add(message + "\n");
    }

    /**
     * Print the messages
     */
    void transmit() {
        messages.add(END_TOKEN);

        // Merge the string into 1
        StringBuilder builder = new StringBuilder();
        for (String message : messages) {
            builder.append(message);
        }

        String mess = builder.toString();
        System.out.println(">>> TRANSMITTING: " + mess);

        // Transmit it
        out.println(builder.toString());
    }
}

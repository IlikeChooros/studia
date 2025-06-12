import java.io.BufferedReader;
import java.io.IOException;

public class MessageReceiver {
    private final BufferedReader reader;
    private final String BEGIN_TOKEN;
    private final String END_TOKEN;
    
    public MessageReceiver(BufferedReader reader) {
        this.reader = reader;
        this.BEGIN_TOKEN = ServerMessenger.BEGIN_TOKEN;
        this.END_TOKEN = ServerMessenger.END_TOKEN;
    } 

    /**
     * Check if the input is ready for the read
     */
    public boolean ready() throws IOException {
        return reader.ready();
    }

    /**
     * Read the message transmitted by ServerMessenger
     */
    public String read() throws IOException {
        StringBuilder builder = new StringBuilder();

        String line = reader.readLine();

        if (line == null) {
            throw new IOException("Connection suddenly closed");
        }

        line += "\n";
        System.out.println("LINE>>>" + line);

        if (line.startsWith(BEGIN_TOKEN)) {
            builder.append(line.substring(BEGIN_TOKEN.length()));
        }
        else {
            builder.append(line);
        }

        while (ready() && !(line = reader.readLine()).endsWith(END_TOKEN)) {
            builder.append(line + "\n");
            System.out.println("LINE>>>" + line + "\n");
        }

        System.out.println("WHOLE:" + builder.toString());
        return builder.toString();
    }
}

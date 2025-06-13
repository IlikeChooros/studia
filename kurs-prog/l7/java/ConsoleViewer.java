import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

import javafx.application.Platform;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Region;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

public class ConsoleViewer extends BorderPane {
    
    private volatile TextFlow commandField = new TextFlow();
    private final TextField textField = new TextField();
    private final ScrollPane scrollPane;
    private Socket socket = null;
    private MessageReceiver receiver = null;
    private ServerMessenger messenger = null;

    private enum MessageType {
        ERROR, RECEIVED, ECHO
    }

    private Text stylizeText(Text t, MessageType type) {
        String color;
        switch (type) {
            case ERROR:
                color = "#ef9a9a";
                break;
            case ECHO:
                color = "#fff9c4";
                break;
            
            case RECEIVED:
            default:
                color = "#e0e0e0";
                break;
        }
        t.setStyle(String.format(
            "-fx-font-weight: normal;" + 
            "-fx-font-size: 18px;" + 
            "-fx-fill: %s;",
        color));

        return t;
    }

    private void appendText(String text, MessageType type) {
        // Add new text object
        synchronized(commandField) {
            commandField.getChildren().add(stylizeText(new Text(text), type));
        }
        synchronized(scrollPane) {
            scrollPane.layout();
            scrollPane.setVvalue(1.0);
            scrollPane.autosize();
        }
    }

    private void setupStyles(Region object, String color, int fontsize) {
        object.setStyle(String.format(
            "-fx-background-color: %s;" +
         "    -fx-border-radius: 5px;" + //
         "    -fx-border-color: transparent;" + //
         "    -fx-text-fill: white;" + 
         "    -fx-font-size: %dpx;",
         color, fontsize
        ));
    }

    public ConsoleViewer() {
        // In the middle, make big text view (as a logger, to see the incoming data from the server)
        // This will be a command-like tool, allowing user to save the tree into a file (on server)
        // And load it, if the user specified a given UUID

        // No buttons, just a text view and text input on the botton for commands, 
        // with simple and elegant UI
        super();
        commandField.setPrefSize(600, 400);
        scrollPane = new ScrollPane(commandField);
        scrollPane.setFitToWidth(true);

        super.setCenter(scrollPane);
        super.setBottom(textField);

        // Stylize the command prompt
        setupStyles(commandField, "#212121", 18);
        setupStyles(textField, "#161616", 20);

        textField.setOnAction((event) -> {
            // Append new text to the console, and clear the text
            String command = textField.getText();
            appendText(command + "\n", MessageType.ECHO);
            textField.setText(null); 

            // Do something with this command
            if (command.equals("connect")) {
                connect();
            }
            else {
                if (messenger == null) {
                    appendText("First call 'connect'\n", MessageType.ERROR);
                    return;
                }

                new Thread() {
                @Override
                    public void run() {
                        // Wysylanie do serwera
                        messenger.begin();
                        messenger.println(command);
                        messenger.transmit();

                        try {
                            String msg = receiver.read();
                            Platform.runLater(() -> appendText(msg, MessageType.RECEIVED));

                            if (msg.trim().equals("bye")) {
                                socket.close();
                                socket = null;
                                messenger = null;
                                receiver = null;
                            }
                        }
                        catch (IOException ex) {
                           Platform.runLater(() -> appendText("Error: " + ex.getMessage() + "\n", MessageType.ERROR));
                        }
                    }
                }.start();
            }
        });
    }

    private void connect() {
        // Check if we are already connected
        if (socket != null) {
            appendText("Already connected", MessageType.ERROR);
            return;
        }

        try {
            socket = new Socket(Server.HOST, Server.PORT);
            messenger = new ServerMessenger(
                new PrintWriter(socket.getOutputStream(), true));
            receiver = new MessageReceiver(
                new BufferedReader(new InputStreamReader(socket.getInputStream())));

            // Read the message upon connecting
            appendText(receiver.read(), MessageType.RECEIVED);
        }
        catch (IOException e) {
            appendText("Error: " + e.getMessage() + "\n", MessageType.ERROR);
        } 
        // catch(IOException e) {
        //     appendText("Error: " + e.getMessage() + "\n");
        // }
    } 
}

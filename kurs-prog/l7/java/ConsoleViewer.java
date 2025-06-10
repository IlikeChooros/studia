import java.io.BufferedReader;
import java.io.Console;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.layout.BorderPane;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

public class ConsoleViewer extends BorderPane {
    
    private final TextFlow commandField = new TextFlow();
    private final TextField textField = new TextField();
    private Socket socket = null;
    private PrintWriter out = null;
    private BufferedReader in = null;

    private void handleCommand(String[] tokens) {
        if (tokens == null) {
            return;
        }

        switch (tokens[0]) {
            case "connect":
                
                break;
        
            default:
                break;
        }
    }

    public ConsoleViewer() {        
        // In the middle, make big text view (as a logger, to see the incoming data from the server)
        // This will be a command-like tool, allowing user to save the tree into a file (on server)
        // And load it, if the user specified a given UUID

        // No buttons, just a text view and text input on the botton for commands, 
        // with simple and elegant UI
        super();
        commandField.setPrefSize(500, 400);
        ScrollPane scroll = new ScrollPane(commandField);
        scroll.setFitToWidth(true);

        super.setCenter(scroll);
        super.setBottom(textField);

        textField.setOnAction((event) -> {
            // Append new text to the console, and clear the text
            String command = textField.getText();
            commandField.getChildren().add(new Text(command + "\n"));
            textField.setText(null); 

            // Scroll down
            scroll.setVvalue(scroll.getVmax());

            // Do something with this command
            handleCommand(command.split(" "));
        });
    }

    private void makeConnection(String datatype) {
        try  {
            Socket socket = new Socket(Server.HOST, Server.PORT); 

            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));


            String text = datatype;

            while (!ServerThread.isEndToken(text)) {
                // Odbieranie z serwera
                System.out.println(in.readLine());

                // text = console.readLine("Enter text: ");

                if (text == null) {
                    break;
                }

                // Wysylanie do serwera
                out.println(text);
            }
            socket.close();
 
        } catch (UnknownHostException ex) {
            System.out.println("Server not found: " + ex.getMessage());
 
        } catch (IOException ex) {
            System.out.println("I/O error: " + ex.getMessage());
        }
    }
}

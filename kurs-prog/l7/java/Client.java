import java.net.*;
import java.io.*;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;


public class Client extends Application {

    @Override
    public void start(Stage primaryStage) {

        ConsoleViewer viewer = new ConsoleViewer();
        Scene scene = new Scene(viewer);
        primaryStage.setScene(scene);
        primaryStage.setTitle("Binary Tree Viewer");
        primaryStage.show();
    }

    public void makeConnection(String datatype) {
        try  {
            Socket socket = new Socket(Server.HOST, Server.PORT); 

            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            Console console = System.console();
            String text = datatype;

            while (!ServerThread.isEndToken(text)) {
                // Odbieranie z serwera
                System.out.println(in.readLine());

                text = console.readLine("Enter text: ");

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
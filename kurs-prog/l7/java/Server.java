import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    public static final int PORT = 4444;
    public static final String HOST = "localhost";


    public Server() {
        try (ServerSocket server = new ServerSocket(PORT)){
            
            System.out.println("Created a new server on: " + HOST + "/" + PORT);

            while (true) {
                Socket connection = server.accept();
                System.out.println("New client");

                (new ServerThread(connection)).start();
            }
        }
        catch(IOException exception) {
            exception.printStackTrace();
        }
    }
}

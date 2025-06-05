import java.io.*;
import java.net.*;
 

public class ServerThread extends Thread {
    private Socket socket;
    private boolean isRunning;
 
    public ServerThread(Socket socket) {
        this.socket = socket;
        this.isRunning = true;
    }

    public static boolean isEndToken(String token) {
        return token.equals("quit") || token.equals("q") 
            || token.equals("exit") || token.equals("bye");
    }

    public void parseCommand(String line) {
        String tokens[] = line.split(" ");

        String mainCommand = "";


        if (tokens.length == 1) {
            mainCommand = tokens[0];

            if (isEndToken(mainCommand)) {
                isRunning = false;
            }

            // Single argument commands
            // print, min, max

            if (mainCommand == "print") {

            }

            return;
        }


        if (tokens.length > 1) {
            mainCommand = tokens[0];

            // Main commands: 
            // add <n1>,<n2>,...
            // delete <n1>,<n2>,...

            // Parse list-like
            if (mainCommand == "add" || mainCommand == "delete") {

            }
        }
        
    }
 
    public void run() {

        try {
             //Odbieranie od socketa
            InputStream input = socket.getInputStream();
            BufferedReader in = new BufferedReader(new InputStreamReader(input));
    
            //Wysylanie do socketa
            OutputStream output = socket.getOutputStream();
            PrintWriter out = new PrintWriter(output, true);
    
            String line;
            do {
                // Odbieranie od socketa
                line = in.readLine();
                // Wypisywanie na serwerze
                System.out.println(line);
                // Wysylanie do socketa
                out.println("-> ("+line+")");
    
            } while (isRunning);
    
            socket.close();

            System.out.println("Connection closed");
        } catch (IOException ex) {
            System.out.println("Server exception: " + ex.getMessage());
            ex.printStackTrace();
        }
    }
}
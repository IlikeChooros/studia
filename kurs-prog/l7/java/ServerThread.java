import java.io.*;
import java.net.*;
 

public class ServerThread extends Thread {
    private Socket socket;
    private TreeManager<?> manager;
 
    public ServerThread(Socket socket) {
        this.socket = socket;
    }

    public static boolean isEndToken(String token) {
        return TreeManager.isEndToken(token);
    }
 
    public void run() {

        try {
             //Odbieranie od socketa
            InputStream input = socket.getInputStream();
            BufferedReader in = new BufferedReader(new InputStreamReader(input));
    
            //Wysylanie do socketa
            OutputStream output = socket.getOutputStream();
            PrintWriter out = new PrintWriter(output, true);
    
            // First line should always be the type of the tree
            String type = TreeManager.getTypeName(in, out);

            if (type.equals("string")) {
                this.manager = new TreeManager<String>(out, in, TreeManager::stringSetter);
            }
            else if (type.equals("int")) {
                this.manager = new TreeManager<Integer>(out, in, TreeManager::ingtegerSetter);
            }
            else  {
                this.manager = new TreeManager<Double>(out, in, TreeManager::doubleSetter);
            }

            this.manager.run();
            socket.close();

            System.out.println("Connection closed");
        } catch (IOException ex) {
            System.out.println("Server exception: " + ex.getMessage());
            ex.printStackTrace();
        }
        catch(Exception ex) {
            System.out.println("Unexpected error: " + ex.getMessage());
            ex.printStackTrace();
        }
    }
}
import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.Function;

public class TreeManager<E extends Comparable<E>> {
    private boolean isRunning;
    private BinaryTree<E> tree;
    private PrintWriter out;
    private BufferedReader in;
    private Function<String, E> setter;
    

    public static String stringSetter(String s) {
        return s;
    }

    public static Integer ingtegerSetter(String s) {
        return Integer.parseInt(s);
    }

    public static Double doubleSetter(String s) {
        return Double.parseDouble(s);
    }

    public TreeManager(PrintWriter out, BufferedReader input, Function<String, E> setter) {
        this.out = out;
        this.in = input;
        this.setter = setter;
        this.isRunning = true;
        this.tree = new BinaryTree<>();

        System.out.println("Created new Tree: " + setter);
    }
    
    public static boolean isEndToken(String token) {
        return token.equals("quit") || token.equals("q") 
            || token.equals("exit") || token.equals("bye");
    }

    public static String getTypeName(BufferedReader in, PrintWriter out) {
        String line = "";
        try {
            while(true) {
                line = in.readLine();

                if (line.equals("double") || line.equals("string") || line.equals("int")) {
                    out.println(">>> setup done");
                    break;
                }

                out.println(">>> Choose valid type: double,string,int");
            };
        }
        catch(IOException exception) {
            out.println(exception.getMessage());
            exception.printStackTrace();
            line = "int";
        }

        return line;
    }

    private void parseCommand(String line) {
        String tokens[] = line.split(" ");
        String mainCommand = "";


        System.out.println(">>> parsing: " + line);


        if (tokens.length == 1) {
            mainCommand = tokens[0];            

            // Single argument commands
            // print, min, max
            switch (mainCommand) {
                case "print":
                    out.println(tree);
                    break;
                case "min":
                    out.println(tree.min());
                    break;
                case "max":
                    out.println(tree.max());
                    break;
            
                default:
                    if (isEndToken(mainCommand)) {
                        isRunning = false;
                        out.println(">>> Exiting...");
                        return;
                    }
                    out.println(">>> Invalid option");
                    break;
            }

            return;
        }


        if (tokens.length > 1) {

            System.out.println(">>> tokens: " + Arrays.toString(tokens));
            mainCommand = tokens[0];

            // Main commands: 
            // add <n1>,<n2>,...
            // delete <n1>,<n2>,...

            // Parse list-like
            boolean isAdd = mainCommand.equals("add");
            if (isAdd || mainCommand.equals("delete")) {
                String values[] = tokens[1].split(",");

                for (String val : values) {
                    try {
                        if (isAdd) {
                            tree.insert(setter.apply(val));
                        } else {
                            tree.delete(setter.apply(val));
                        }
                    }   
                    catch(NumberFormatException exception) {}
                }

                String operation = "add";
                if (!isAdd) {
                    operation = "delete";
                }

                out.println(">>> Performed: " + operation);
                return;
            }
        }
        
        out.println(">>> Unhandled command: " + line);
    }

    public void run() {
        System.out.println("Running");
        try{
            String line;
            while (isRunning) {
                line = in.readLine();
                parseCommand(line);
            }
        }
        catch(IOException exception) {
            out.println(exception.getMessage());
            exception.printStackTrace();
        }
    }
}

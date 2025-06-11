import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Iterator;
import java.util.function.Function;

public class TreeManager<E extends Comparable<E>> {
    private boolean isRunning;
    private BinaryTree<E> tree;
    private PrintWriter out;
    private BufferedReader in;
    private Function<String, E> setter;
    private final Commands commands;

    private final String MAX_COMMAND = "max";
    private final String MIN_COMMAND = "min";
    private final String PRINT_COMMAND = "print";
    private final String ADD_COMMAND = "add";
    private final String DELETE_COMMAND = "delete";
    private final String SEARCH_COMMAND = "search";
    

    public static String stringSetter(String s) {
        return s;
    }

    public static Integer ingtegerSetter(String s) {
        try {
            return Integer.parseInt(s);
        }
        catch (NumberFormatException exception) {
            return null;
        }
    }

    public static Double doubleSetter(String s) {
        try {
            return Double.parseDouble(s);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    public static Option.Setter<Double> getDoubleSetter() {
        return (args) -> {
            try {
                return Double.parseDouble(args[0]);
            }
            catch (NumberFormatException e) {}
            return 0.0;
        };
    }

    private void initCommands() {
        commands.add(new Option<E>(
            new String[]{"/add"}, 
            "Add new elements to the tree, usage: /add <n1>,<n2>,...",
            null, null, null
        ));
    }

    public TreeManager(PrintWriter out, BufferedReader input, Function<String, E> setter) {
        this.out = out;
        this.in = input;
        this.setter = setter;
        this.isRunning = true;
        this.tree = new BinaryTree<>();
        this.commands = new Commands((message) -> {
            out.println(message);
        }); 

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
            // print, min, max, exit, etc.
            switch (mainCommand) {
                case PRINT_COMMAND:
                    out.println(tree);
                    break;
                case MIN_COMMAND:
                    out.println(tree.min());
                    break;
                case MAX_COMMAND:
                    out.println(tree.max());
                    break;
            
                default:
                    if (isEndToken(mainCommand)) {
                        isRunning = false;
                        out.println(">>> Exiting...");
                        return;
                    }
                    out.println(">>> Invalid option");
            }

            return;
        }


        if (tokens.length > 1) {

            System.out.println(">>> tokens: " + Arrays.toString(tokens));
            mainCommand = tokens[0];

            // Main commands: 
            // add <n1>,<n2>,...
            // delete <n1>,<n2>,...
            // search n1

            // Parse list-like
            boolean isAdd = mainCommand.equals(ADD_COMMAND);
            switch (mainCommand) {
                case ADD_COMMAND:
                case DELETE_COMMAND:
                    String values[] = tokens[1].split(",");
                    for (String val : values) {
                        if (isAdd) {
                            tree.insert(setter.apply(val));
                        } else {
                            tree.delete(setter.apply(val));
                        }
                    }
                    out.println(">>> Performed: " + mainCommand);             
                    break;
                
                case SEARCH_COMMAND:
                    Iterator<E> it = tree.find(setter.apply(tokens[1]));
                    

                default:
                    out.println(">>> Unhandled command: " + line);
                    break;
            }
            return;
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

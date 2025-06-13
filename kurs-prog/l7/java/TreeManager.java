import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.function.Function;

import java.util.Vector;

public class TreeManager<E extends Comparable<E>> {
    private boolean isRunning;
    private BinaryTree<E> tree;
    private Function<String, E> setter;
    private final Commands commands;
    private final ServerMessenger messenger;
    private final MessageReceiver receiver;

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

    private Option.Validator getListValidator(Function<String, E> setter) {
        return (String[] args) -> {
            if (args == null) {
                throw new ValidationException("No arguments provided");
            }

            for (String arg : args) {
                String[] tokens = arg.split(",");

                for (String token : tokens) {
                    if (setter.apply(token) == null) {
                        // TODO: make it more specific
                        throw new ValidationException("Invalid argument: " + token);
                    }      
                }
            }
        };
    }

    private Option.Converter<Vector<E>> getListConverter(Function<String, E> setter) {
        return (args) -> {
            Vector<E> values = new Vector<>();
            for (String arg : args) {
                String[] tokens = arg.split(",");

                for (String token : tokens) {
                    E value = setter.apply(token);
                    if (value != null) {
                        values.add(value);
                    }       
                }
            }
            return values;
        };
    }

    /**
     * Initialize the commands
     */
    private void initCommands() {
        commands.add(new Option<Vector<E>>(
            new String[]{"/add", "/a"}, 
            "Add new elements to the tree, usage: /add <n1>,<n2>,...",
            (Vector<E> data) -> {
                int prev_size = tree.size();
                for (E value : data) {
                    tree.insert(value);
                }
                messenger.println(">>> Inserted total of: " + (tree.size() - prev_size) + " element(s)");
            }, 
            getListValidator(setter), getListConverter(setter)
        ));

        commands.add(new Option<Vector<E>>(
            new String[]{"/delete", "/del"}, 
            "Delete given value of tree, usage: /del <n1>,<n2>,...",
            (Vector<E> data) -> {
                int prev_size = tree.size();
                for (E value : data) {
                    tree.delete(value);
                }
                messenger.println(">>> Removed total of: " + (prev_size - tree.size()) + " element(s)");
            }, 
            getListValidator(setter), getListConverter(setter)
        ));

        commands.add(new Option<Boolean>(
            new String[]{"/print", "/p"},
            "Print the binary tree, usage: /print",
            (__) -> {
                messenger.println(">>> " + tree.toString());
            },
            Option.defaultValidator(), Option.booleanConverter()
        ));

        commands.add(new Option<Boolean>(
            new String[]{"/max"},
            "Get the maximum element of the tree (rightmost), usage: /max",
            (__) -> {
                E max = tree.max();
                String message = "none";

                if (max != null) {
                    message = max.toString();
                }
                messenger.println(">>> " + message);
            },
            Option.defaultValidator(), Option.booleanConverter()
        ));

        commands.add(new Option<Boolean>(
            new String[]{"/min"},
            "Get the minimum element of the tree (leftmost), usage: /min",
            (__) -> {
                E min = tree.min();
                String message = "none";

                if (min != null) {
                    message = min.toString();
                }
                messenger.println(">>> " + message);
            },
            Option.defaultValidator(), Option.booleanConverter()
        ));

        commands.add(new Option<Boolean>(
            new String[] {"/quit", "/exit", "/q", "/e"},
            "Exit the connection, usage: /quit", 
            (__) -> {
                messenger.println("bye");
                isRunning = false;
            },
            Option.defaultValidator(), Option.booleanConverter()
        ));
    }

    public TreeManager(PrintWriter out, BufferedReader input, Function<String, E> setter) {
        this.setter = setter;
        this.isRunning = true;
        this.tree = new BinaryTree<>();
        this.receiver = new MessageReceiver(input);
        this.messenger = new ServerMessenger(out);
        this.commands = new Commands((message) -> {
            this.messenger.println(message);
        }); 
        initCommands();
    }

    /**
     * Initialize the server, with given type of the tree
     */
    public static String getTypeName(BufferedReader in, PrintWriter out) throws IOException {
        String line = "";
        ServerMessenger messenger = new ServerMessenger(out);
        MessageReceiver receiver = new MessageReceiver(in);

        messenger.begin();
        messenger.println(">>> Connected. Choose type of the tree: double,string,int");
        messenger.transmit();

        while(true) {
            line = receiver.read().trim();
            messenger.begin();

            if (line.equals("double") || line.equals("string") || line.equals("int")) {
                messenger.println(">>> setup done");
                messenger.transmit();
                break;
            }

            messenger.println(">>> Choose valid type: double,string,int (instead of:" + line + ")");
            messenger.transmit();
        };

        return line;
    }

    /**
     * Parse the received command, and return the response
     */
    private void parseCommand(String line) {
        messenger.begin();

        try {
            commands.parse(line.trim().split(" "));
        } catch (ValidationException e) {
            messenger.println(">>> " + e.getMessage());
        }

        messenger.transmit();
    }

    /**
     * Run the receiver, and wait for commads
     */
    public void run() {
        try{
            String line;
            while (isRunning) {
                line = receiver.read();

                if (line == null) {
                    return;
                }

                parseCommand(line);
            }
        }
        catch(IOException exception) {
            System.out.println(exception.getMessage());
        }
    }
}

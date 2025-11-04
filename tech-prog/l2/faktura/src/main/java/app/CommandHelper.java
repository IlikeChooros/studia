package app;

import java.util.Arrays;
import java.util.List;
import java.util.Vector;
import java.util.function.Function;
import java.util.stream.Stream;

import org.jline.reader.Completer;
import org.jline.reader.Candidate;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;
import org.jline.builtins.Completers.TreeCompleter;
import static org.jline.builtins.Completers.TreeCompleter.node;

public final class CommandHelper {

    private CommandHelper() {
        // Prevent instantiation
    }

    /** Context variable name for the completer. */
    public static final String COMPLETER_CONTEXT_VAR = "CONTEXT";

    enum CoreCommands {
        /** List of all available commands. */
        ADD("add"), LIST("list"), UPDATE("update"),
        /** Fuck checkstyle. */
        REMOVE("remove"), LIST_MODULES("list-modules"), HELP("help"),

        /** Command to save an invoice to pdf. */
        EXPORT_INVOICE("export-invoice");

        /** Command alias. */
        private final String alias;

        CoreCommands(final String name) {
            this.alias = name;
        }

        @Override
        public String toString() {
            return alias;
        }

        public static String[] commandNames(final boolean includeHelp) {
            CoreCommands[] commands = CoreCommands.values();
            Vector<String> names = new Vector<>();
            for (CoreCommands command : commands) {
                if (includeHelp || !command.equals(CoreCommands.HELP)) {
                    names.add(command.toString());
                }
            }
            return names.toArray(new String[0]);
        }
    }

    enum Modules {
        /** List of all available modules. */
        PERSON, FIRM, PRODUCT, INVOICE;

        @Override
        public String toString() {
            return name().toLowerCase();
        }

        public static String[] moduleNames() {
            Modules[] modules = Modules.values();
            String[] names = new String[modules.length];
            for (int i = 0; i < modules.length; i++) {
                names[i] = modules[i].toString();
            }
            return names;
        }
    }

    static class ContextAwareCompleter implements Completer {
        /** Map of context names to their respective completers. */
        private final Completer defaultCompleter;

        /** Data associated with the current context. */
        private Function<String, Vector<?>> filter;

        ContextAwareCompleter() {
            defaultCompleter = new TreeCompleter(
                node("help", node((Object[]) Stream.concat(
                    Arrays.stream(Modules.moduleNames()),
                    Arrays.stream(CoreCommands.commandNames(false)))
                    .toArray(String[]::new))),
                node("list", node((Object[]) Modules.moduleNames())),
                node("add", node((Object[]) Modules.moduleNames())),
                node("update", node((Object[]) Modules.moduleNames())),
                node("remove", node((Object[]) Modules.moduleNames())),
                node("list-modules"),
                node("export-invoice")
            );
        }

        @Override
        public void complete(final LineReader reader,
            final ParsedLine line, final List<Candidate> candidates) {
            // Get current context from reader variables
            String context = (String) reader.getVariable(COMPLETER_CONTEXT_VAR);
            if (context == null || context.equals("default")) {
                defaultCompleter.complete(reader, line, candidates);
            } else {
                // Dynamic context-based completion
                if (filter != null) {
                    // use the current word the user is typing as the prefix
                    String prefix = line == null ? "" : line.word();
                    Vector<?> data = filter.apply(prefix);
                    for (Object item : data) {
                        candidates.add(new Candidate(item.toString()));
                    }
                }
                // Do nothing if no filter is set
            }
        }

        public void setContextFilter(
            final Function<String, Vector<?>> filterFn) {
            this.filter = filterFn;
        }
    }

    /**
     * Get a context-aware completer.
     * @return the context-aware completer
     */
    public static ContextAwareCompleter getCompleter() {
        return new ContextAwareCompleter();
    }

    /**
     * Convert a string to a Modules enum.
     * @param moduleStr the module string
     * @return the Modules enum or null if not found
     */
    public static Modules toModule(final String moduleStr) {
        for (Modules mod : Modules.values()) {
            if (mod.name().equalsIgnoreCase(moduleStr)) {
                return mod;
            }
        }
        return null;
    }

    /**
     * Check if a command string is an exit command.
     * @param commandStr the command string
     * @return true if it is an exit command
     */
    public static boolean isExitCommand(final String commandStr) {
        switch (commandStr) {
            case "q":
            case "quit":
            case "exit":
                return true;
            default:
                return false;
        }
    }

    /**
     * Convert a string to a CoreCommands enum.
     * @param commandStr the command string
     * @return the CoreCommands enum or null if not found
     */
    public static CoreCommands toCommand(final String commandStr) {
        switch (commandStr) {
            case "add":
                return CoreCommands.ADD;
            case "list":
                return CoreCommands.LIST;
            case "update":
                return CoreCommands.UPDATE;
            case "remove":
                return CoreCommands.REMOVE;
            case "list-modules":
                return CoreCommands.LIST_MODULES;
            case "help":
                return CoreCommands.HELP;
            case "export-invoice":
                return CoreCommands.EXPORT_INVOICE;
            default:
                if (isExitCommand(commandStr)) {
                    System.exit(0);
                }
                return null;
        }
    }

    /**
     * Display help information for a command or module.
     * @param command the command or module to get help for
     * @param messenger the messenger to use for output
     */
    public static void help(final Messenger messenger, final String command) {

        if (command == null || command.isEmpty()) {
            messenger.help("Available modules:");
            messenger.help("add <module>    - Add a new element to the module");
            messenger.help("list <module>   - "
                + "Display the list of elements in the module");
            messenger.help("list-modules    - Display available modules");
            messenger.help("update <module> - Update an element in the module");
            messenger.help("remove <module> - "
                + "Remove an element from the module");
            messenger.help("export-invoice - Export the current invoice");
            messenger.help("Type 'help <command> | <module>'"
                + " to get more information about the command/module.");
            return;
        }

        for (Modules mod : Modules.values()) {
            if (mod.name().equalsIgnoreCase(command)) {
                switch (mod) {
                    case PERSON:
                        messenger.help("Module person - "
                            + "managing natural persons.");
                        messenger.help("Available commands: add person, "
                            + "list person, update person <id>, "
                            + "remove person <id>");
                        break;
                    case FIRM:
                        messenger.help("Module firm - "
                            + "managing companies.");
                        messenger.help("Available commands: add firm, "
                            + "list firm, update firm <id>, remove firm <id>");
                        break;
                    case PRODUCT:
                        messenger.help("Module product - "
                            + "managing products.");
                        messenger.help("Available commands: add product,"
                            + "list product, update product, remove product");
                        break;
                    case INVOICE:
                        messenger.help("Module invoice - "
                            + "managing invoices.");
                        messenger.help("Available commands: add invoice, "
                            + "list invoice, update invoice, remove invoice");
                        break;
                    default:
                        break;
                }
                return;
            }
        }

        for (CoreCommands cmd : CoreCommands.values()) {
            if (cmd.toString().equalsIgnoreCase(command)) {
                switch (cmd) {
                    case ADD:
                        messenger.help("add - add a new element");
                        messenger.help("Syntax: add <module>");
                        break;
                    case LIST:
                        messenger.help("list - display all elements"
                            + " of the module");
                        messenger.help("Syntax: list <module>");
                        break;
                    case UPDATE:
                        messenger.help("update - update an element");
                        messenger.help("Syntax: update <module> <id>");
                        break;
                    case REMOVE:
                        messenger.help("remove - remove an element");
                        messenger.help("Syntax: remove <module> <id>");
                        break;
                    case LIST_MODULES:
                        messenger.help("list-modules - "
                            + "display available modules.");
                        messenger.help("Syntax: list-modules");
                        break;
                    case EXPORT_INVOICE:
                        messenger.help("export-invoice - "
                            + "export the current invoice to a PDF file.");
                        messenger.help("Syntax: export-invoice [<filename> |"
                            + " <id> <filename>]");
                        break;
                    case HELP:
                    default:
                        messenger.help("help - display help "
                            + "for commands or modules.");
                        messenger.help("Syntax: help [command | module]");
                        break;
                }
                return;
            }
        }

        messenger.error("Unknown command: (mess)" + command);
    }
}

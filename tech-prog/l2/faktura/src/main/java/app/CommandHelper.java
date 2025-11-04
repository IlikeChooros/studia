package app;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jline.reader.Completer;
import org.jline.reader.Candidate;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;
import org.jline.reader.impl.completer.StringsCompleter;

public final class CommandHelper {

    private CommandHelper() {
        // Prevent instantiation
    }

    /** Context variable name for the completer. */
    public static final String COMPLETER_CONTEXT_VAR = "CONTEXT";

    enum CoreCommands {
        /** List of all available commands. */
        ADD, LIST, UPDATE, REMOVE, LIST_MODULES, HELP
    }

    enum Modules {
        /** List of all available modules. */
        PERSON, FIRM, PRODUCT, INVOICE
    }

    static class ContextAwareCompleter implements Completer {
        /** Map of context names to their respective completers. */
        private final Map<String, Completer>
            contextCompleters = new HashMap<>();

        ContextAwareCompleter() {
            contextCompleters.put("default",
                new StringsCompleter("add", "list", "update",
                    "remove", "list-modules", "help", "quit", "exit"));
            contextCompleters.put("user",
                new StringsCompleter("admin", "guest", "user1", "user2"));
        }

        @Override
        public void complete(final LineReader reader,
            final ParsedLine line, final List<Candidate> candidates) {
            // Get current context from reader variables
            String context = (String) reader.getVariable(COMPLETER_CONTEXT_VAR);
            if (context == null) {
                context = "default";
            }

            // Use the appropriate completer for this context
            Completer contextCompleter = contextCompleters.getOrDefault(
                context, contextCompleters.get("default"));
            contextCompleter.complete(reader, line, candidates);
        }
    }

    /**
     * Get a context-aware completer.
     * @return the context-aware completer
     */
    public static Completer getCompleter() {
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
            case "q":
            case "quit":
            case "exit":
                System.exit(0);
            default:
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
            if (cmd.name().equalsIgnoreCase(command)) {
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

        messenger.error("Unknown command: " + command);
    }
}

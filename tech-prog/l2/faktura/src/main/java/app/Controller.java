package app;

import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.impl.history.DefaultHistory;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;

public class Controller {

    /** The terminal for user input and output. */
    private Terminal terminal;

    /** The line reader for user input. */
    private LineReader reader;

    /** The messenger for formatted messages. */
    private Messenger messenger;

    /** The context-aware completer for command suggestions. */
    private CommandHelper.ContextAwareCompleter contextCompleter;

    /** The product manager for managing products. */
    private final ProductManager productManager;

    /** The person manager for managing persons. */
    private final PersonManager personManager;

    /** The firm manager for managing firms. */
    private final FirmManager firmManager;

    /** The invoice manager for managing invoices. */
    private final InvoiceManager invoiceManager;

    /** The add view for adding data. */
    private final ViewAdd addView;

    /** The list view for listing data. */
    private final ViewList listView;

    /** The remove view for removing data. */
    private final ViewRemove removeView;

    /** The update view for updating data. */
    private final ViewUpdate updateView;

    /**
     * Initializes the Controller.
     */
    public Controller() {
        try {
            terminal = TerminalBuilder.builder()
                .system(true)
                .build();
            messenger = new Messenger(terminal);
            contextCompleter = CommandHelper.getCompleter();
            reader = LineReaderBuilder.builder()
                .terminal(terminal)
                .history(new DefaultHistory())
                .completer(contextCompleter)
                .build();
        } catch (Exception e) {
            if (messenger != null) {
                messenger.error(
                    "Error initializing terminal: " + e.getMessage());
            } else {
                System.err.println(
                    "Error initializing terminal: " + e.getMessage());
            }
            e.printStackTrace();
            System.exit(1);
        }

        productManager = MockData.getProducts();
        personManager = MockData.getPersons();
        firmManager = MockData.getFirms();
        invoiceManager = new InvoiceManager();

        addView = new ViewAdd(
            messenger,
            contextCompleter,
            reader,
            productManager,
            personManager,
            firmManager,
            invoiceManager
        );

        listView = new ViewList(
            messenger,
            contextCompleter,
            reader,
            productManager,
            personManager,
            firmManager,
            invoiceManager
        );

        removeView = new ViewRemove(
            messenger,
            contextCompleter,
            reader,
            productManager,
            personManager,
            firmManager,
            invoiceManager
        );

        updateView = new ViewUpdate(
            messenger,
            contextCompleter,
            reader,
            productManager,
            personManager,
            firmManager,
            invoiceManager
        );
    }

    private String readInput(final String prompt) {
        return reader.readLine(prompt).trim();
    }

    private void listModules() {
        messenger.help("Available modules:");
        for (CommandHelper.Modules mod : CommandHelper.Modules.values()) {
            messenger.help("- " + mod.name().toLowerCase());
        }
    }

    private void handleList(final CommandHelper.Modules module) {
        switch (module) {
            case PERSON:
                listView.listPersons();
                break;
            case FIRM:
                listView.listFirms();
                break;
            case PRODUCT:
                listView.listProducts();
                break;
            case INVOICE:
                listView.listInvoices();
                break;
            default:
                messenger.error("Unknown module to list.");
                break;
        }
    }

    private void handleAdd(final CommandHelper.Modules module) {
        switch (module) {
            case PERSON:
                addView.addPerson();
                break;
            case FIRM:
                addView.addFirm();
                break;
            case PRODUCT:
                addView.addProduct();
                break;
            case INVOICE:
                addView.addInvoice();
                break;
            default:
                messenger.error("Unknown module to add.");
                break;
        }
    }

    private void handleRemove(final CommandHelper.Modules module) {
        switch (module) {
            case PERSON:
                removeView.removePerson();
                break;
            case FIRM:
                removeView.removeFirm();
                break;
            case PRODUCT:
                removeView.removeProduct();
                break;
            case INVOICE:
                removeView.removeInvoice();
                break;
            default:
                messenger.error("Unknown module to remove.");
                break;
        }
    }

    private void handleUpdate(final CommandHelper.Modules module) {
        switch (module) {
            case PERSON:
                updateView.updatePerson();
                break;
            case FIRM:
                updateView.updateFirm();
                break;
            case PRODUCT:
                updateView.updateProduct();
                break;
            case INVOICE:
                updateView.updateInvoice();
                break;
            default:
                messenger.error("Unknown module to update.");
                break;
        }
    }

    private void commandParser(final String input) {
        String[] parts = input.split(" ");
        if (parts.length == 0) {
            return;
        }

        CommandHelper.CoreCommands command = CommandHelper.toCommand(parts[0]);
        String rest = parts.length > 1 ? parts[1] : "";
        CommandHelper.Modules module =
            parts.length > 1 ? CommandHelper.toModule(parts[1]) : null;

        if (command == null) {
            messenger.error("Unknown command: " + parts[0]);
            return;
        }

        reader.setVariable(CommandHelper.COMPLETER_CONTEXT_VAR, "context");
        contextCompleter.setContextFilter(null);
        switch (command) {
            case ADD:
                if (module != null) {
                    handleAdd(module);
                } else {
                    messenger.error("No module specified for add command.");
                }
                break;
            case LIST:
                if (module != null) {
                    handleList(module);
                } else {
                    // List all modules
                    listModules();
                }
                break;
            case UPDATE:
                if (module != null) {
                    handleUpdate(module);
                } else {
                    messenger.error("No module specified for update command.");
                }
                break;
            case REMOVE:
                if (module != null) {
                    handleRemove(module);
                } else {
                    messenger.error("No module specified for remove command.");
                }
                break;
            case EXPORT_INVOICE:
                if (parts.length != 3) {
                    messenger.error("Invalid syntax for export-invoice."
                            + " Use export-invoice <id> <filename>");
                    break;
                }
                boolean ok = PdfInvoiceGenerator.saveToPdf(parts[2], 0.23f,
                    invoiceManager.getById(Integer.parseInt(parts[1])));
                if (ok) {
                    messenger.success("Invoice exported successfully.");
                } else {
                    messenger.error("Failed to export invoice.");
                }
                break;
            case LIST_MODULES:
                listModules();
                break;
            default:
            case HELP:
                CommandHelper.help(messenger, rest);
                break;
        }

        reader.setVariable(CommandHelper.COMPLETER_CONTEXT_VAR, null);
        contextCompleter.setContextFilter(null);
    }

    /**
     * The main program loop.
     */
    public final void run() {
        messenger.info("Invoice Management System (IMS) v0.1.0");
        messenger.info("Type 'help' to get started.");
        while (true) {
            String input = readInput(Messenger.fmtInput(""));
            try {
                if (input != null && !input.isEmpty()) {
                    commandParser(input);
                }
            } catch (Exception e) {
                messenger.error("Error processing command: " + e.getMessage());
            }
        }
    }
}

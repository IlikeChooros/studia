package app;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;
import java.util.function.Function;

import org.jline.console.impl.Builtins.Command;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.impl.history.DefaultHistory;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;

public class Controller {

    /**
     * The invoice manager.
     */
    private InvoiceManager man = new InvoiceManager(null);

    /** The terminal for user input and output. */
    private Terminal terminal;

    /** The line reader for user input. */
    private LineReader reader;

    /** The messenger for formatted messages. */
    private Messenger messenger;

    /**
     * Initializes the Controller.
     */
    public Controller() {
        try {
            terminal = TerminalBuilder.builder()
                .system(true)
                .build();
            reader = LineReaderBuilder.builder()
                .terminal(terminal)
                .history(new DefaultHistory())
                .completer(CommandHelper.getCompleter())
                .build();
            messenger = new Messenger(terminal);
        } catch (Exception e) {
            messenger.error("Error initializing terminal: " + e.getMessage());
            System.exit(1);
        }
    }

    /**
     * The global data pool.
     */
    private final DataPool pool = MockData.generate();

    private String prompt(final String msg,
        final Function<String, Boolean> f, final String errmsg,
        final String defaultValue) {

        while (true) {
            String input = readInput(Messenger.fmtInput(msg));

            if (input.isEmpty() && defaultValue != null) {
                return defaultValue;
            }

            if (f == null) {
                return input;
            }

            if (f.apply(input)) {
                return input;
            } else {
                messenger.error(errmsg);
            }
        }
    }

    private String readInput(final String prompt) {
        return reader.readLine(prompt);
    }

    private void addPerson() {
        String firstName = prompt("Enter first name", null, null, null);
        String lastName = prompt("Enter last name", null, null, null);
        String pesel = prompt("Enter PESEL", Validator::isValidPESEL,
                "Invalid PESEL", null);
        pool.addPerson(new Person(firstName, lastName, pesel));
        messenger.success(String.format("Person %s %s (%s) added",
            firstName, lastName, pesel));
    }

    private void addProduct() {
        String name = prompt("Enter product name", null, null, null);
        String priceStr = prompt("Enter unit price",
            Validator::isValidFloat, "Invalid price", null);
        String unit = prompt("Enter unit of measure", Validator::isValidUnit,
                "Invalid unit, available: " + Validator.getAllUnits(), null);
        float price = Float.parseFloat(priceStr);
        pool.addProduct(new Product(name, price, unit));
        messenger.success(String.format(
            "Product %s (%.2f / %s) added", name, price, unit));
    }

    private void addFirm() {
        String name = prompt("Enter firm name", null, null, null);
        String address = prompt("Enter firm address", null, null, null);
        String taxID = prompt("Enter tax ID", Validator::isValidNIP,
            "Invalid tax ID", null);
        pool.addFirm(new Firm(name, address, taxID));
        messenger.success(String.format(
            "Firm %s (%s) added", name, taxID));
    }

    private void addInvoice() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

        String creationDateStr = prompt(
            "Enter creation date yyyy-MM-dd (default: today)",
            Validator::isValidDate, null, sdf.format(new Date()));

        String paymentDateStr = prompt(
            "Enter payment date yyyy-MM-dd (default: today)",
            Validator::isValidDate, null, sdf.format(new Date()));
        // Simple by id selection for buyer and seller
        String buyerId = prompt("Enter buyer index",
            Validator::isValidInt, "Invalid index", null);
        String sellerId = prompt("Enter seller index",
            Validator::isValidInt, "Invalid index", null);

        Person buyer = pool.getPersons().get(Integer.parseInt(buyerId));
        Person seller = pool.getPersons().get(Integer.parseInt(sellerId));

        Date creationDate = null;
        Date paymentDate = null;

        try {
            creationDate = sdf.parse(creationDateStr);
            paymentDate = sdf.parse(paymentDateStr);
        } catch (Exception e) {
            messenger.error("Error parsing dates.");
            return;
        }

        // Add products with quantities
        Vector<QuantProduct> quantProducts = new Vector<>();
        while (true) {
            String productId = prompt(
                "Enter product index (or 'done' to finish)",
                input -> input.equalsIgnoreCase("done")
                    || Validator.isValidInt(input),
                "Invalid index", null);
            if (productId.equalsIgnoreCase("done")) {
                break;
            }
            String quantityStr = prompt("Enter quantity",
                Validator::isValidInt, "Invalid quantity", null);

            Product product = pool.getProducts().
                get(Integer.parseInt(productId));
            int quantity = Integer.parseInt(quantityStr);
            quantProducts.add(new QuantProduct(product, quantity));
        }

        // Convert Vector to array
        QuantProduct[] productsArray = new QuantProduct[quantProducts.size()];
        quantProducts.toArray(productsArray);
        pool.addInvoice(new Invoice(creationDate, paymentDate,
            buyer, seller, productsArray, null));
        messenger.success("Invoice added successfully.");
    }

    private void listModules() {
        messenger.help("Available modules:");
        for (CommandHelper.Modules mod : CommandHelper.Modules.values()) {
            messenger.help("- " + mod.name().toLowerCase());
        }
    }

    private void list(final Vector<?> items,
        final Function<Object, String> formatter) {
        for (int i = 0; i < items.size(); i++) {
            String fmt;
            if (formatter != null) {
                fmt = formatter.apply(items.get(i));
            } else {
                fmt = items.get(i).toString();
            }
            messenger.data(String.format("%d: %s", i, fmt));
        }
    }

    private void handleList(final CommandHelper.Modules module) {
        Vector<?> items = null;
        Function<Object, String> formatter = null;

        switch (module) {
            case PERSON:
                items = pool.getPersons();
                break;
            case FIRM:
                items = pool.getFirms();
                break;
            case PRODUCT:
                items = pool.getProducts();
                break;
            case INVOICE:
                items = pool.getInvoices();
                formatter = obj -> {
                    Invoice inv = (Invoice) obj;
                    return String.format(
                        "Invoice: \n\t"
                        + " - Firm: %s\n\t"
                        + " - Buyer: %s\n\t"
                        + " - Seller: %s\n\t"
                        + " - Creation Date: %s\n\t"
                        + " - Payment Date: %s\n\t"
                        + " - Total: %s",
                        inv.getFirm().toString(),
                        inv.getBuyer().toString(),
                        inv.getSeller().toString(),
                        Formatter.formatDate(inv.getCreationDate()),
                        Formatter.formatDate(inv.getPaymentDate()),
                        Formatter.formatCurrency(inv.getTotal()));
                };
                break;
            default:
                break;
        }

        if (items == null) {
            messenger.error("Unknown module to list.");
            return;
        }

        if (items.isEmpty()) {
            messenger.info("No items found.");
            return;
        }

        list(items, formatter);
    }

    private void handleAdd(final CommandHelper.Modules module) {
        switch (module) {
            case PERSON:
                addPerson();
                break;
            case FIRM:
                addFirm();
                break;
            case PRODUCT:
                addProduct();
                break;
            case INVOICE:
                addInvoice();
                break;
            default:
                messenger.error("Unknown module to add.");
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
                    // Call the appropriate update method based on the module
                }
                break;
            case REMOVE:
                if (module != null) {
                    // Call the appropriate remove method based on the module
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
    }

    /**
     * The main program loop.
     */
    public final void run() {
        messenger.info("Invoice Management System (IMS) v0.1.0");
        messenger.info("Type 'help' to get started.");
        while (true) {
            String input = readInput(Messenger.fmtInput(""));
            if (input != null && !input.isEmpty()) {
                commandParser(input);
            }
        }
    }
}

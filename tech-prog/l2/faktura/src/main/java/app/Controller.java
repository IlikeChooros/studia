package app;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;
import java.util.function.Function;

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
        return reader.readLine(prompt).trim();
    }

    private static String[] convert(final Vector<?> vec) {
        String[] arr = new String[vec.size()];
        for (int i = 0; i < vec.size(); i++) {
            arr[i] = vec.get(i).toString();
        }
        return arr;
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

        contextCompleter.setContextFilter(prompt -> {
            Vector<Validator.Units> results = new Vector<>();
            for (Validator.Units u : Validator.Units.values()) {
                if (u.name().toLowerCase().startsWith(prompt.toLowerCase())) {
                    results.add(u);
                }
            }
            return results;
        });
        String unit = prompt("Enter unit of measure", Validator::isValidUnit,
                "Invalid unit, available: " + Validator.getAllUnits(), null);
        float price = Float.parseFloat(priceStr);
        Product product = new Product(name, price, unit);
        pool.addProduct(product);
        messenger.success(String.format(
            "Product %s added", product.toString()));
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

    private Object select(
        final String msg,
        final Function<String, Vector<?>> searchFn,
        final Function<Integer, Object> indexFn) {
        // If the user specifies an index, return that person
        // else perform a search by name and make
        // a suggestion for the first match
        // other matches added as tab completions
        reader.setVariable(CommandHelper.COMPLETER_CONTEXT_VAR, "select");
        contextCompleter.setContextFilter(searchFn);

        while (true) {
            String data = prompt(msg, null, null, null).trim();

            if (CommandHelper.isExitCommand(data)) {
                return null;
            }

            // Try to parse as index
            try {
                int index = Integer.parseInt(data);
                return indexFn.apply(index);
            } catch (Exception e) {
                // Not an index, proceed to search
            }

            Vector<?> matches = searchFn.apply(data);
            if (matches.isEmpty()) {
                messenger.info("No matches found for: " + data);
                continue;
            }

            if (matches.size() == 1) {
                return matches.get(0);
            } else {
                messenger.info(String.format(
                    "%d matches found", matches.size()));
            }
        }
    }

    private Person selectPerson(final String msg) {
        return (Person) select(msg, prompt -> pool.searchPersons(prompt),
            index -> pool.getPersons().get(index));
    }

    private Firm selectFirm(final String msg) {
        return (Firm) select(msg, prompt -> pool.searchFirms(prompt),
            index -> pool.getFirms().get(index));
    }

    private Product selectProduct(final String msg) {
        return (Product) select(msg, prompt -> pool.searchProducts(prompt),
            index -> pool.getProducts().get(index));
    }

    private void addInvoice() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

        String creationDateStr = prompt(
            "Enter creation date yyyy-MM-dd (default: today)",
            Validator::isValidDate, null, sdf.format(new Date()));

        String paymentDateStr = prompt(
            "Enter payment date yyyy-MM-dd (default: today)",
            Validator::isValidDate, null, sdf.format(new Date()));

        Person buyer = selectPerson("Enter buyer");
        if (buyer == null) {
            messenger.info("Invoice creation cancelled.");
            return;
        }

        Person seller = selectPerson("Enter seller");
        if (seller == null) {
            messenger.info("Invoice creation cancelled.");
            return;
        }

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

            Product product = selectProduct(
                "Enter product (q to finish): ");

            if (product == null) {
                break;
            }

            String quantityStr = prompt("Enter quantity",
                Validator::isValidFloat, "Invalid quantity", null);

            float quantity = Float.parseFloat(quantityStr);
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
                        "Invoice(ID: %d): \n\t"
                        + "  - Firm: %s\n\t"
                        + "  - Buyer: %s\n\t"
                        + "  - Seller: %s\n\t"
                        + "  - Creation Date: %s\n\t"
                        + "  - Payment Date: %s\n\t"
                        + "  - Total: %s",
                        inv.getId(),
                        (inv.getFirm() != null ? inv.getFirm().toString()
                            : "Unknown"),
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
        reader.setVariable(CommandHelper.COMPLETER_CONTEXT_VAR, null);
    }

    private void remove(
        final String msg,
        final String successMsg,
        final Function<String, Vector<?>> searchFn,
        final Function<Integer, Object> indexFn,
        final Function<Object, Void> removeFn) {

        Object item = select(msg, searchFn, indexFn);
        if (item != null) {
            removeFn.apply(item);
            messenger.success(successMsg);
        } else {
            messenger.info("Removal cancelled.");
        }
    }

    private void removePerson() {
        remove("Enter person to remove: ",
            "Person removed successfully.",
            prompt -> pool.searchPersons(prompt),
            index -> pool.getPersons().get(index),
            item -> {
                pool.removePerson((Person) item);
                return null;
            });
    }

    private void removeFirm() {
        remove("Enter firm to remove: ",
            "Firm removed successfully.",
            prompt -> pool.searchFirms(prompt),
            index -> pool.getFirms().get(index),
            item -> {
                pool.removeFirm((Firm) item);
                return null;
            });
    }

    private void removeProduct() {
        remove("Enter product to remove: ",
            "Product removed successfully.",
            prompt -> pool.searchProducts(prompt),
            index -> pool.getProducts().get(index),
            item -> {
                pool.removeProduct((Product) item);
                return null;
            });
    }

    private void removeInvoice() {
        remove("Enter invoice to remove: ",
            "Invoice removed successfully.",
            prompt -> {
                Vector<Invoice> results = new Vector<>();
                for (Invoice inv : pool.getInvoices()) {
                    if (Integer.toString(inv.getId())
                        .startsWith(prompt)) {
                        results.add(inv);
                    }
                }
                return results;
            },
            index -> pool.getInvoices().get(index),
            item -> {
                pool.removeInvoice((Invoice) item);
                return null;
            });
    }

    private void handleRemove(final CommandHelper.Modules module) {
        switch (module) {
            case PERSON:
                removePerson();
                break;
            case FIRM:
                removeFirm();
                break;
            case PRODUCT:
                removeProduct();
                break;
            case INVOICE:
                removeInvoice();
                break;
            default:
                messenger.error("Unknown module to remove.");
                break;
        }
        reader.setVariable(CommandHelper.COMPLETER_CONTEXT_VAR, null);
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
                    // Call the appropriate update method based on the module
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
                    return;
                }
                PdfInvoiceGenerator.saveToPdf(parts[2], 0.23f,
                    pool.getInvoices().get(Integer.parseInt(parts[1])));
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

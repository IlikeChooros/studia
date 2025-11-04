package app;

import java.io.BufferedReader;
import java.util.Vector;
import java.util.function.Function;

public class Controller {

    enum CoreCommands {
        /** List of all available commands. */
        ADD, LIST, UPDATE, REMOVE, LIST_MODULES, HELP
    }

    enum Modules {
        /** List of all available modules. */
        PERSON, FIRM, PRODUCT, INVOICE
    }

    /**
     * The invoice manager.
     */
    private InvoiceManager man = new InvoiceManager(null);

    /**
     * The global data pool.
     */
    public static final DataPool POOL = new DataPool();

    private static String prompt(final String msg,
        final Function<String, Boolean> f, final String errmsg) {
        Messenger.input(msg);
        String input = readInput();

        if (f == null) {
            return input;
        }

        if (f.apply(input)) {
            return input;
        } else {
            Messenger.error(errmsg);
            return prompt(msg, f, errmsg);
        }
    }

    private static String readInput() {
        // StringBuilder sb = new StringBuilder();
        // BufferedReader br = new BufferedReader(
        //     new java.io.InputStreamReader(System.in));
        // try {
        //     String line;
        //     while ((line = br.readLine()) != null) {
        //         sb.append(line);
        //     }
        // } catch (Exception e) {
        //     e.printStackTrace();
        // }
        // return sb.toString();
        return System.console().readLine();
    }

    private static void addPerson() {
        String firstName = prompt("Enter first name", null, null);
        String lastName = prompt("Enter last name", null, null);
        String pesel = prompt("Enter PESEL", Validator::isValidPESEL,
                "Invalid PESEL");
        POOL.addPerson(new Person(firstName, lastName, pesel));
        Messenger.success(String.format("Person %s %s (%s) added",
            firstName, lastName, pesel));
    }

    private static void addProduct() {
        String name = prompt("Enter product name", null, null);
        String priceStr = prompt("Enter unit price",
            Validator::isValidFloat, "Invalid price");
        String unit = prompt("Enter unit of measure", Validator::isValidUnit,
                "Invalid unit, available: " + Validator.getAllUnits());
        float price = Float.parseFloat(priceStr);
        POOL.addProduct(new Product(name, price, unit));
        Messenger.success(String.format(
            "Product %s (%.2f / %s) added", name, price, unit));
    }

    private static void addFirm() {
        String name = prompt("Enter firm name", null, null);
        String address = prompt("Enter firm address", null, null);
        String taxID = prompt("Enter tax ID", Validator::isValidNIP,
            "Invalid tax ID");
        POOL.addFirm(new Firm(name, address, taxID));
        Messenger.success(String.format(
            "Firm %s (%s) added", name, taxID));
    }

    private static void listModules() {
        Messenger.data("Available modules:");
        for (Modules mod : Modules.values()) {
            Messenger.help("- " + mod.name().toLowerCase());
        }
    }

    private static void list(final Vector<?> items,
        final Function<Object, String> formatter) {
        for (int i = 0; i < items.size(); i++) {
            Messenger.data(String.format("%d: %s", i,
                formatter.apply(items.get(i))));
        }
    }

    private void help(final String command) {

        if (command == null || command.isEmpty()) {
            Messenger.help("Available modules:");
            Messenger.help("add <module>    - Add a new element to the module");
            Messenger.help("list <module>   - "
                + "Display the list of elements in the module");
            Messenger.help("list-modules    - Display available modules");
            Messenger.help("update <module> - Update an element in the module");
            Messenger.help("remove <module> - "
                + "Remove an element from the module");
            Messenger.help("Type 'help <command> | <module>'"
                + " to get more information about the command/module.");
            return;
        }

        for (Modules mod : Modules.values()) {
            if (mod.name().equalsIgnoreCase(command)) {
                switch (mod) {
                    case PERSON:
                        Messenger.help("Module person - "
                            + "managing natural persons.");
                        Messenger.help("Available commands: add person, "
                            + "list person, update person <id>, remove person <id>");
                        break;
                    case FIRM:
                        Messenger.help("Module firm - "
                            + "managing companies.");
                        Messenger.help("Available commands: add firm, "
                            + "list firm, update firm <id>, remove firm <id>");
                        break;
                    case PRODUCT:
                        Messenger.help("Module product - "
                            + "managing products.");
                        Messenger.help("Available commands: add product,"
                            + "list product, update product, remove product");
                        break;
                    case INVOICE:
                        Messenger.help("Module invoice - "
                            + "managing invoices.");
                        Messenger.help("Available commands: add invoice, "
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
                        Messenger.help("add - add a new element");
                        Messenger.help("Syntax: add <module>");
                        break;
                    case LIST:
                        Messenger.help("list - display all elements"
                            + " of the module");
                        Messenger.help("Syntax: list <module>");
                        break;
                    case UPDATE:
                        Messenger.help("update - update an element");
                        Messenger.help("Syntax: update <module> <id>");
                        break;
                    case REMOVE:
                        Messenger.help("remove - remove an element");
                        Messenger.help("Syntax: remove <module> <id>");
                        break;
                    case LIST_MODULES:
                        Messenger.help("list-modules - "
                            + "display available modules.");
                        Messenger.help("Syntax: list-modules");
                        break;
                    case HELP:
                    default:
                        Messenger.help("help - display help "
                            + "for commands or modules.");
                        Messenger.help("Syntax: help [command | module]");
                        break;
                }
                return;
            }
        }

        Messenger.error("Unknown command: " + command);
    }

    private Modules toModule(final String moduleStr) {
        for (Modules mod : Modules.values()) {
            if (mod.name().equalsIgnoreCase(moduleStr)) {
                return mod;
            }
        }
        return null;
    }

    private CoreCommands toCommand(final String commandStr) {
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

    private void handleList(final Modules module) {
        Function<Object, String> formatter = null;
        Vector<?> items = new Vector<>();

        switch (module) {
            case PERSON:
                formatter = person -> String.format("Name: %s, Id: %s",
                    ((Person) person).getName(),
                    ((Person) person).getIdNumber());
                items = POOL.getPersons();
                break;
            case FIRM:
                formatter = firm -> String.format("Name: %s, NIP: %s",
                    ((Firm) firm).getName(),
                    ((Firm) firm).getNIP());
                items = POOL.getFirms();
                break;
            case PRODUCT:
                formatter = product -> String.format(
                    "Name: %s, Price: %.2f / %s",
                    ((Product) product).getName(),
                    ((Product) product).getCost(),
                    ((Product) product).getUnit());
                items = POOL.getProducts();
                break;
            case INVOICE:
                // listInvoices();
                break;
            default:
                break;
        }

        if (formatter == null) {
            Messenger.error("Unknown module to list.");
            return;
        }
        if (items.isEmpty()) {
            Messenger.info("No items found.");
            return;
        }

        list(items, formatter);
    }

    private void handleAdd(final Modules module) {
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
                // addInvoice();
                break;
            default:
                Messenger.error("Unknown module to add.");
                break;
        }
    }

    private void commandParser(final String input) {
        String[] parts = input.split(" ");
        if (parts.length == 0) {
            return;
        }

        CoreCommands command = toCommand(parts[0]);
        String rest = parts.length > 1 ? parts[1] : "";
        Modules module = parts.length > 1 ? toModule(parts[1]) : null;

        switch (command) {
            case ADD:
                if (module != null) {
                    handleAdd(module);
                } else {
                    Messenger.error("No module specified for add command.");
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
            case HELP:
                help(rest);
                break;
            default:
                Messenger.error("Unknown command: " + parts[0]);
                break;
        }
    }

    /**
     * The main program loop.
     */
    public final void run() {
        Messenger.info("Invoice Management System (IMS) v0.1.0");
        Messenger.info("Type 'help' to get started.");
        while (true) {
            Messenger.input("");
            String input = System.console().readLine();
            if (input != null && !input.isEmpty()) {
                commandParser(input);
            }
        }
    }
}

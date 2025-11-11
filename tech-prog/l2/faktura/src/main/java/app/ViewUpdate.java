package app;

import java.util.Map;
import java.util.Vector;
import java.util.function.Function;

import org.jline.reader.LineReader;

public class ViewUpdate extends View {
    /** Manager for product data. */
    private ProductManager productManager;
    /** Manager for person data. */
    private PersonManager personManager;
    /** Manager for firm data. */
    private FirmManager firmManager;
    /** Manager for invoice data. */
    private InvoiceManager invoiceManager;

    ViewUpdate(final Messenger mess,
            final CommandHelper.ContextAwareCompleter completer,
            final LineReader lineReader,
            final ProductManager pMan,
            final PersonManager perMan,
            final FirmManager fMan,
            final InvoiceManager invoiceMan) {
        super(mess, completer, lineReader);
        this.productManager = pMan;
        this.personManager = perMan;
        this.firmManager = fMan;
        this.invoiceManager = invoiceMan;
    }

    private void update(final String msg,
        final Function<String, Vector<?>> searchFn,
        final Function<Integer, Object> indexFn,
        final Function<Object, Void> updateFn) {
        Object item = select(msg, searchFn, indexFn, null);
        if (item != null) {
            updateFn.apply(item);
        }
    }

    private void updateByManager(final ManagerLike<?> manager) {
        Function<Object, Void> updateFn = obj -> {
            // By default, use the BaseData update method
            DataLike dataObj = (DataLike) obj;
            Map<String, String> fields = dataObj.getAllFieldNames();
            Function<String, Vector<?>> contextFilter = filter -> {
                Vector<String> results = new Vector<>();
                for (String field : fields.keySet()) {
                    if (field.contains(filter)) {
                        results.add(field);
                    }
                }
                return results;
            };

            boolean cancelled = true;
            while (true) {
                // Enable context filter for field selection
                contextCompleter.setContextFilter(contextFilter);
                String field = prompt(
                    "Enter field to update (or 'q' to finish): ",
                    input -> fields.containsKey(input)
                        || CommandHelper.isExitCommand(input),
                    "Invalid field name. Available fields: "
                        + String.join(", ", fields.keySet()),
                    null);
                if (CommandHelper.isExitCommand(field)) {
                    break;
                }

                // Disable context filter for value input
                contextCompleter.setContextFilter(null);
                String value = prompt(
                    "Enter new value for " + fields.get(field) + ": ",
                    null, null, null);

                if (dataObj.updateField(field, value)) {
                    cancelled = false;
                    messenger.success(
                        String.format("Field %s updated successfully.", field));
                } else {
                    messenger.error(
                        String.format("Failed to update field %s.", field));
                }
            }

            if (cancelled) {
                messenger.info("No changes made.");
            } else {
                messenger.success("All changes applied.");
            }
            return null;
        };

        update("Find item to update: ",
            manager::search, manager::getById, updateFn);
    }

    /** Prompts user to update a person. */
    public final void updatePerson() {
        updateByManager(personManager);
    }

    /** Prompts user to update a product. */
    public final void updateProduct() {
        updateByManager(productManager);
    }

    /** Prompts user to update a firm. */
    public final void updateFirm() {
        updateByManager(firmManager);
    }

    /** Prompts user to update an invoice. */
    public final void updateInvoice() {

        Function<String, Vector<?>> searchFn = filter -> {
            // Only allow to search by ID for invoices
            try {
                Integer id = Integer.parseInt(filter);
                Vector<Invoice> results = new Vector<>();
                Invoice inv = invoiceManager.getById(id);
                if (inv != null) {
                    results.add(inv);
                }
                return results;
            } catch (NumberFormatException e) {
                return new Vector<>();
            }
        };

        Function<Object, Void> updateFn = obj -> {
            // By default, use the DataLike update method
            Invoice dataObj = (Invoice) obj;
            Map<String, String> fields = dataObj.getAllFieldNames();
            Function<String, Vector<?>> contextFilter = filter -> {
                Vector<String> results = new Vector<>();
                for (String field : fields.keySet()) {
                    if (field.contains(filter)) {
                        results.add(field);
                    }
                }
                return results;
            };

            boolean cancelled = true;
            while (true) {
                // Enable context filter for field selection
                contextCompleter.setContextFilter(contextFilter);
                String field = prompt(
                    "Enter field to update (or 'q' to finish): ",
                    input -> fields.containsKey(input)
                        || CommandHelper.isExitCommand(input),
                    "Invalid field name. Available fields: "
                        + String.join(", ", fields.keySet()),
                    null);
                if (CommandHelper.isExitCommand(field)) {
                    break;
                }

                // Handle products field separately
                if (field.equals("products")) {
                    String operation = prompt(
                        "Enter operation (add/remove/update): ",
                        input -> input.equals("add")
                            || input.equals("remove") || input.equals("update"),
                        "Invalid operation. Enter 'add', 'remove' or 'update'.",
                        null);

                    switch (operation) {
                        case "add":
                            // Select product to add
                            Product productToAdd = (Product) select(
                                "Enter name of product to add: ",
                                productManager::search,
                                productManager::getById,
                                null);

                            float quantity = 0;
                            if (productToAdd != null) {
                                String quantityStr = prompt(
                                    "Enter quantity: ",
                                    input -> Validator.isValidInt(input),
                                    "Invalid quantity."
                                    + "Enter a non-negative integer.",
                                    null);
                                quantity = Float.parseFloat(quantityStr);

                                QuantProduct quantProduct
                                    = new QuantProduct(productToAdd, quantity);

                                invoiceManager.addProductToInvoice(
                                    dataObj, quantProduct);
                                messenger.success(
                                    "Product added successfully.");
                                cancelled = false;
                            } else {
                                messenger.info("Add product cancelled.");
                            }
                            break;
                        case "remove":
                            // Select product to remove
                            Product productToRemove = (Product) select(
                                "Enter name of product to remove: ",
                                productManager::search,
                                productManager::getById,
                                null);
                            if (productToRemove != null) {
                                invoiceManager.removeProductFromInvoice(
                                    dataObj, productToRemove);
                                messenger.success(
                                    "Product removed successfully.");
                                cancelled = false;
                            } else {
                                messenger.info("Remove product cancelled.");
                            }
                            break;
                        case "update":
                            // Select product to update
                            Product productToUpdate = (Product) select(
                                "Enter name of product to update: ",
                                productManager::search,
                                productManager::getById,
                                null);
                            if (productToUpdate != null) {
                                String newQuantityStr = prompt(
                                    "Enter new quantity: ",
                                    input -> Validator.isValidInt(input),
                                    "Invalid quantity."
                                    + "Enter a non-negative integer.",
                                    null);
                                int newQuantity =
                                    Integer.parseInt(newQuantityStr);
                                invoiceManager.updateProductQuantity(
                                    dataObj, productToUpdate, newQuantity);
                                messenger.success(
                                    "Product quantity updated successfully.");
                                cancelled = false;
                            } else {
                                messenger.info("Update product cancelled.");
                            }
                            break;
                        default:
                            messenger.error("Unknown operation.");
                    }
                    continue;
                }

                // Handle special fields that require
                // selection from another manager
                ManagerLike<?> manager = null;
                if (field.equals("buyer") || field.equals("seller")) {
                    manager = personManager;
                } else if (field.equals("firm")) {
                    manager = firmManager;
                }

                if (manager != null) {
                    // Select the person/firm
                    DataLike item = (DataLike) select(
                        "Enter new value: ",
                        manager::search,
                        manager::getById,
                        null);
                    if (item != null) {
                        if (dataObj.updateField(field, item)) {
                            cancelled = false;
                            messenger.success(
                                String.format("Field %s updated successfully.",
                                field));
                        } else {
                            messenger.error(
                                String.format("Failed to update field %s.",
                                field));
                        }
                    } else {
                        messenger.info("Update cancelled for this field.");
                    }
                    continue;
                } else {
                    // Disable context filter for value input
                    contextCompleter.setContextFilter(null);
                    String value = prompt(
                        "Enter new value for " + fields.get(field) + ": ",
                        null, null, null);

                    if (dataObj.updateField(field, value)) {
                        cancelled = false;
                        messenger.success(
                            String.format("Field %s updated successfully.",
                            field));
                    } else {
                        messenger.error(
                            String.format("Failed to update field %s.", field));
                    }
                }
            }

            if (cancelled) {
                messenger.info("No changes made.");
            } else {
                messenger.success("All changes applied.");
            }
            return null;
        };

        update("Find item to update: ",
            searchFn, invoiceManager::getById, updateFn);
    }
}

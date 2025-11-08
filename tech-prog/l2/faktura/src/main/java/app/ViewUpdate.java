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
        // To be implemented
    }
}

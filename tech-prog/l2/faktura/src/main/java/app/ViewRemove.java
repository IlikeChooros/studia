package app;

import org.jline.reader.LineReader;

public class ViewRemove extends View {
    /** Manager for product data. */
    private ProductManager productManager;
    /** Manager for person data. */
    private PersonManager personManager;
    /** Manager for firm data. */
    private FirmManager firmManager;
    /** Manager for invoice data. */
    private InvoiceManager invoiceManager;

    ViewRemove(final Messenger mess,
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

    @SuppressWarnings("unchecked")
    private <T extends DataLike> void remove(
        final String msg,
        final String successMsg,
        final ManagerLike<T> manager) {

        T item = (T) select(msg, manager::search, manager::getById, null);
        if (item != null) {
            manager.remove(item);
            messenger.success(successMsg);
        } else {
            messenger.info("Removal cancelled.");
        }
    }

    /** Prompts user to remove a person. */
    public final void removePerson() {
        remove("Enter name or PESEL of person to remove: ",
            "Person removed successfully.",
            personManager);
    }

    /** Prompts user to remove a product. */
    public final void removeProduct() {
        remove("Enter name of product to remove: ",
            "Product removed successfully.",
            productManager);
    }

    /** Prompts user to remove a firm. */
    public final void removeFirm() {
        remove("Enter name or NIP of firm to remove: ",
            "Firm removed successfully.",
            firmManager);
    }

    /** Prompts user to remove an invoice. */
    public final void removeInvoice() {
        remove("Enter invoice number to remove: ",
            "Invoice removed successfully.",
            invoiceManager);
    }
}

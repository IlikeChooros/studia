package app;

import java.util.Vector;
import java.util.function.Function;

import org.jline.reader.LineReader;

public class ViewList extends View {
    /** Manager for product data. */
    private ProductManager productManager;
    /** Manager for person data. */
    private PersonManager personManager;
    /** Manager for firm data. */
    private FirmManager firmManager;
    /** Manager for invoice data. */
    private InvoiceManager invoiceManager;

    ViewList(final Messenger mess,
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

    private void list(final Vector<?> items,
            final Function<Object, String> formatter,
            final String errMsg) {
        if (items.isEmpty()) {
            messenger.info(errMsg != null
                    && !errMsg.isEmpty() ? errMsg : "No items found.");
            return;
        }

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

    /** Lists all persons in the system. */
    public final void listPersons() {
        list(personManager.getAll(), null, "No persons found.");
    }

    /** Lists all products in the system. */
    public final void listProducts() {
        list(productManager.getAll(), null, "No products found.");
    }

    /** Lists all firms in the system. */
    public final void listFirms() {
        list(firmManager.getAll(), null, "No firms found.");
    }

    /** Lists all invoices in the system. */
    public final void listInvoices() {
        list(invoiceManager.getAll(),
                obj -> {
                    Invoice inv = (Invoice) obj;
                    return String.format(
                            "Invoice: \n\t"
                                    + "  - Firm: %s\n\t"
                                    + "  - Buyer: %s\n\t"
                                    + "  - Seller: %s\n\t"
                                    + "  - Creation Date: %s\n\t"
                                    + "  - Payment Date: %s\n\t"
                                    + "  - Total: %s",
                            (inv.getFirm() != null ? inv.getFirm().toString()
                                    : "Unknown"),
                            inv.getBuyer().toString(),
                            inv.getSeller().toString(),
                            Formatter.formatDate(inv.getCreationDate()),
                            Formatter.formatDate(inv.getPaymentDate()),
                            Formatter.formatCurrency(inv.getTotal()));
                },
                "No invoices found.");
    }
}

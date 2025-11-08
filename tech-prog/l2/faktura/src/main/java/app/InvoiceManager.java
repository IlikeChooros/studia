package app;

import java.util.Vector;

public class InvoiceManager extends ManagerHelper<Invoice> {
    InvoiceManager(final Vector<Invoice> coll) {
        super(coll);
    }

    InvoiceManager() {
        super();
    }
}

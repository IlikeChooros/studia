package app;

import java.util.Date;

public class InvoiceManager {
    private Invoice invoice;

    public InvoiceManager(Invoice invoice) {
        this.invoice = invoice;
    }

    public void setInvoice(Invoice invoice) {
        this.invoice = invoice;
    }

    public void saveToPdf() {
        
    }
}

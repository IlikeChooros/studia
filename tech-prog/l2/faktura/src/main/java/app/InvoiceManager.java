package app;

import java.util.Date;

public class InvoiceManager {
    private Invoice invoice;

    public InvoiceManager(Invoice invoice) {
        this.invoice = invoice;
    }

    public void SetInvoice(Invoice invoice) {
        this.invoice = invoice;
    }

    // Print to stdout
    public void Display() {

    }

    public void SaveToPdf() {
        
    }
}

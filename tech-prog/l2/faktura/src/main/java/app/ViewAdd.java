package app;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import org.jline.reader.LineReader;

public class ViewAdd extends View {
    /** Manager for product data. */
    private ProductManager productManager;
    /** Manager for person data. */
    private PersonManager personManager;
    /** Manager for firm data. */
    private FirmManager firmManager;

    /** Manager for invoice data. */
    private InvoiceManager invoiceManager;

    ViewAdd(final Messenger mess,
        final CommandHelper.ContextAwareCompleter completer,
        final LineReader lineReader,
        final ProductManager pMan,
        final PersonManager perMan,
        final FirmManager fMan,
        final InvoiceManager invoiceMan
    ) {
        super(mess, completer, lineReader);
        this.productManager = pMan;
        this.personManager = perMan;
        this.firmManager = fMan;
        this.invoiceManager = invoiceMan;
    }

    /** Prompts user to enter a person's details. */
    public final void addPerson() {
        String firstName = prompt("Enter first name", null, null, null);
        String lastName = prompt("Enter last name", null, null, null);
        String pesel = prompt("Enter PESEL", Validator::isValidPESEL,
                "Invalid PESEL", null);
        personManager.add(new Person(firstName, lastName, pesel));
        messenger.success(String.format("Person %s %s (%s) added",
            firstName, lastName, pesel));
    }

    /** Prompts user to enter a product's details. */
    public final void addProduct() {
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
        productManager.add(product);
        messenger.success(String.format(
            "Product %s added", product.toString()));
    }

    /** Prompts user to enter a firm's details. */
    public final void addFirm() {
        String name = prompt("Enter firm name", null, null, null);
        String address = prompt("Enter firm address", null, null, null);
        String taxID = prompt("Enter tax ID", Validator::isValidNIP,
            "Invalid tax ID", null);
        firmManager.add(new Firm(name, address, taxID));
        messenger.success(String.format(
            "Firm %s (%s) added", name, taxID));
    }

    /** Prompts user to enter an invoice's details. */
    public final void addInvoice() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

        String creationDateStr = prompt(
            "Enter creation date yyyy-MM-dd (default: today)",
            Validator::isValidDate, null, sdf.format(new Date()));

        String paymentDateStr = prompt(
            "Enter payment date yyyy-MM-dd (default: today)",
            Validator::isValidDate, null, sdf.format(new Date()));

        Person buyer = defaultSelect("Enter buyer", personManager);
        if (buyer == null) {
            messenger.info("Invoice creation cancelled.");
            return;
        }

        Person seller = defaultSelect("Enter seller", personManager);
        if (seller == null) {
            messenger.info("Invoice creation cancelled.");
            return;
        }

        Firm firm = (Firm) select("Enter firm (leave empty for none): ",
            firmManager::search, firmManager::getById, "q");

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

            Product product = defaultSelect(
                "Enter product (q to finish): ", productManager);

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
        invoiceManager.add(new Invoice(creationDate, paymentDate,
            buyer, seller, productsArray, firm));
        messenger.success("Invoice added successfully.");
    }
}

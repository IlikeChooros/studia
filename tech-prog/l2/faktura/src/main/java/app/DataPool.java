package app;

import java.util.Vector;

/**
 * Contains all data classes created at runtime (represents a
 * simple in-memory DB).
 * Provides methods to add and retrieve Person, Firm and Product instances.
 */
public class DataPool {
    /**
     * The collection of Person objects.
     */
    private final Vector<Person> persons;

    /**
     * The collection of Firm objects.
     */
    private final Vector<Firm> firms;

    /**
     * The collection of Product objects.
     */
    private final Vector<Product> products;

    /**
     * The collection of Invoice objects.
     */
    private final Vector<Invoice> invoices;

    /**
     * Constructs an empty DataPool.
     */
    public DataPool() {
        persons = new Vector<>();
        firms = new Vector<>();
        products = new Vector<>();
        invoices = new Vector<>();
    }

    /**
     * Add a person to the pool.
     *
     * @param person the Person to add; must not be null
     */
    public final void addPerson(final Person person) {
        persons.add(person);
    }

    /**
     * Add a firm to the pool.
     *
     * @param firm the Firm to add; must not be null
     */
    public final void addFirm(final Firm firm) {
        firms.add(firm);
    }

    /**
     * Add a product to the pool.
     *
     * @param product the Product to add; must not be null
     */
    public final void addProduct(final Product product) {
        products.add(product);
    }

    /**
     * Add an invoice to the pool.
     *
     * @param invoice the Invoice to add; must not be null
     */
    public final void addInvoice(final Invoice invoice) {
        invoices.add(invoice);
    }

    /**
     * Retrieve a Person from the pool by index.
     *
     * @param index the index of the Person to retrieve
     * @return the Person at the given index
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    public final Person getPerson(final int index) {
        return persons.get(index);
    }

    /**
     * Retrieve a Firm from the pool by index.
     *
     * @param index the index of the Firm to retrieve
     * @return the Firm at the given index
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    public final Firm getFirm(final int index) {
        return firms.get(index);
    }

    /**
     * Retrieve a Product from the pool by index.
     *
     * @param index the index of the Product to retrieve
     * @return the Product at the given index
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    public final Product getProduct(final int index) {
        return products.get(index);
    }

    /**
     * Get the collection of Person objects.
     *
     * @return the vector of Person objects
     */
    public final Vector<Person> getPersons() {
        return persons;
    }

    /**
     * Get the collection of Firm objects.
     *
     * @return the vector of Firm objects
     */
    public final Vector<Firm> getFirms() {
        return firms;
    }

    /**
     * Get the collection of Product objects.
     *
     * @return the vector of Product objects
     */
    public final Vector<Product> getProducts() {
        return products;
    }

    /**
     * Get the collection of Invoice objects.
     *
     * @return the vector of Invoice objects
     */
    public final Vector<Invoice> getInvoices() {
        return invoices;
    }
}

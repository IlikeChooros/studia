package app;

import java.util.Vector;

import app.Validator.Units;

/**
 * Contains method to generate mock data for testing purposes.
 */
public final class MockData {

    private MockData() {
        // Prevent instantiation
    }

    /**
     * Generates a DataPool filled with mock data.
     *
     * @return a DataPool instance containing mock persons, firms, and products
     */
    public static PersonManager getPersons() {
        PersonManager personManager = new PersonManager(new Vector<Person>());
        Person person1 = new Person("John", "Doe", "12345678901");
        Person person2 = new Person("Jane", "Smith", "10987654321");
        personManager.add(person1);
        personManager.add(person2);
        return personManager;
    }

    /**
     * Generates a DataPool filled with mock data.
     *
     * @return a DataPool instance containing mock persons, firms, and products
     */
    public static FirmManager getFirms() {
        FirmManager firmManager =
            new FirmManager(new Vector<Firm>());
        Firm firm1 =
            new Firm("Tech Solutions", "TS1234567890", "5829582011");
        Firm firm2 =
            new Firm("Business Corp", "BC0987654321", "1234567890");
        firmManager.add(firm1);
        firmManager.add(firm2);
        return firmManager;
    }

    /**
     * Generates a DataPool filled with mock data.
     *
     * @return a DataPool instance containing mock persons, firms, and products
     */
    public static ProductManager getProducts() {
        ProductManager productManager =
            new ProductManager(new Vector<Product>());
        Product product1 = new Product("Laptop", 1500.00f,
            Units.SZT.toString());
        Product product2 = new Product("Smartphone", 800.00f,
            Units.SZT.toString());
        productManager.add(product1);
        productManager.add(product2);
        return productManager;
    }
}

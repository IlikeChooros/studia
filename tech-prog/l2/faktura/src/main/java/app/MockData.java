package app;

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
    public static DataPool generate() {
        DataPool pool = new DataPool();

        // Add some persons
        Person person1 = new Person("John", "Doe", "12345678901");
        Person person2 = new Person("Jane", "Smith", "10987654321");
        pool.addPerson(person1);
        pool.addPerson(person2);

        // Add some firms
        Firm firm1 = new Firm("Tech Solutions", "TS123456", "1234567890");
        Firm firm2 = new Firm("Business Corp", "BC654321", "9876543210");
        pool.addFirm(firm1);
        pool.addFirm(firm2);

        // Add some products
        Product product1 = new Product("Laptop",
            1500.00f, Units.SZT.toString());
        Product product2 = new Product("Smartphone",
            800.00f, Units.SZT.toString());
        pool.addProduct(product1);
        pool.addProduct(product2);

        return pool;
    }
}

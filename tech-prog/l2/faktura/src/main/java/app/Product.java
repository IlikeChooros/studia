package app;

/**
 * Represents a product with a name, cost and unit of measure.
 */
public class Product extends BaseData {
    /** Product name. */
    private String name;
    /** Product cost (unit price). */
    private float cost;
    /** Unit of measure for the product (e.g. pcs, kg). */
    private String unit;

    /**
     * Create a new Product.
     *
     * @param name the product name
     * @param cost the product cost (unit price)
     * @param unit the unit of measure
     */
    Product(final String name, final float cost, final String unit) {
        this.name = name;
        this.cost = cost;
        this.unit = unit;
    }

    /**
     * Get the product name.
     *
     * @return the name of the product
     */
    public String getName() {
        return name;
    }

    /**
     * Get the product cost (unit price).
     *
     * @return the cost of the product
     */
    public float getCost() {
        return cost;
    }

    /**
     * Get the unit of measure for the product.
     *
     * @return the unit of measure
     */
    public String getUnit() {
        return unit;
    }

    @Override
    public final String toString() {
        return name + ", " + Formatter.formatCurrency(cost)
            + " / " + unit;
    }
}

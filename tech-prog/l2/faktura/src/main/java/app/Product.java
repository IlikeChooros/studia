package app;

/**
 * Represents a product with a name, cost and unit of measure.
 */
public class Product implements DataLike {
    /** Product name. */
    private String name;
    /** Product cost (unit price). */
    private float cost;
    /** Unit of measure for the product (e.g. pcs, kg). */
    private String unit;

    /**
     * Create a new Product.
     *
     * @param prodName the product name
     * @param prodCost the product cost (unit price)
     * @param prodUnit the unit of measure
     */
    Product(final String prodName,
        final float prodCost, final String prodUnit) {
        this.name = prodName;
        this.cost = prodCost;
        this.unit = prodUnit;
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

    @Override
    public final boolean updateField(final String field,
        final Object... values) {
        if (values.length == 0) {
            return false;
        }

        switch (field) {
            case "name":
                if (values[0].getClass() != String.class) {
                    return false;
                }
                this.name = (String) values[0];
                return true;
            case "cost":
                if (values[0].getClass() == String.class) {
                    try {
                        this.cost = Float.parseFloat((String) values[0]);
                        return true;
                    } catch (NumberFormatException e) {
                        return false;
                    }
                }
                if (values[0].getClass() != Float.class) {
                    return false;
                }
                this.cost = (Float) values[0];
                return true;
            case "unit":
                if (values[0].getClass() != String.class) {
                    return false;
                }
                this.unit = (String) values[0];
                return true;
            default:
                return false;
        }
    }

    @Override
    public final java.util.Map<String, String> getAllFieldNames() {
        java.util.Map<String, String> fieldNames = new java.util.HashMap<>();
        fieldNames.put("name", "Product Name");
        fieldNames.put("cost", "Product Cost");
        fieldNames.put("unit", "Unit of Measure");
        return fieldNames;
    }

    @Override
    public final Object getValue(final String fieldName) {
        switch (fieldName) {
            case "name":
                return name;
            case "cost":
                return cost;
            case "unit":
                return unit;
            default:
                return null;
        }
    }
}

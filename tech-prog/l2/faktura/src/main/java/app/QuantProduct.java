package app;

import java.util.LinkedHashMap;
import java.util.Map;

public class QuantProduct extends BaseData {

    /** Product associated with the quantity. */
    private Product product;

    /** Quantity of the product. */
    private float quantity;

    /**
     * Create a new QuantProduct.
     *
     * @param prod the product
     * @param quant the quantity of the product
     */
    public QuantProduct(final Product prod, final float quant) {
        this.product = prod;
        this.quantity = quant;
    }

    /**
     * Get the cumulative cost.
     *
     * @return the cumulative cost
     */
    public float getCumCost() {
        return quantity * product.getCost();
    }

    /**
     * Get the product.
     *
     * @return the product
     */
    public Product getProduct() {
        return product;
    }

    /**
     * Get the quantity of the product.
     *
     * @return the quantity of the product
     */
    public float getQuantity() {
        return quantity;
    }

    @Override
    public final String toString() {
        return product.toString() + ", Quantity: " + quantity
            + ", Total: " + Formatter.formatCurrency(getCumCost());
    }

    @Override
    public final Map<String, String> getAllFieldNames() {
        Map<String, String> fieldMap = new LinkedHashMap<>();
        fieldMap.put("product", "Product");
        fieldMap.put("quantity", "Quantity");
        return fieldMap;
    }

    @Override
    public final boolean updateField(final String fieldName,
        final Object... values) {
        switch (fieldName) {
            case "product":
                if (values[0].getClass() != Product.class) {
                    return false;
                }
                this.product = (Product) values[0];
                return true;
            case "quantity":
                if (values[0].getClass() != Float.class) {
                    return false;
                }
                this.quantity = (Float) values[0];
                return true;
            default:
                return false;
        }
    }

    @Override
    public final Object getValue(final String fieldName) {
        switch (fieldName) {
            case "product":
                return product;
            case "quantity":
                return quantity;
            default:
                return null;
        }
    }
}

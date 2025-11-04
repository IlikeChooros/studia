package app;

public class QuantProduct implements DataLike {

    /** Product associated with the quantity. */
    private Product product;

    /** Quantity of the product. */
    private int quantity;

    /**
     * Create a new QuantProduct.
     *
     * @param prod the product
     * @param quant the quantity of the product
     */
    public QuantProduct(final Product prod, final int quant) {
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
    public int getQuantity() {
        return quantity;
    }

    @Override
    public final String toString() {
        return product.toString() + ", Quantity: " + quantity
            + ", Total: " + Formatter.formatCurrency(getCumCost());
    }
}

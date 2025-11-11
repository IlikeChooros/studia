package app;

import java.util.Collection;
import java.util.Vector;

public class InvoiceManager extends ManagerHelper<Invoice> {
    InvoiceManager(final Vector<Invoice> coll) {
        super(coll);
    }

    InvoiceManager() {
        super();
    }

    /** Adds a product to an invoice.
     * @param invoice The invoice to update
     * @param product The product to add
     */
    public final void addProductToInvoice(final Invoice invoice,
        final QuantProduct product) {
        Collection<QuantProduct> products =
            new Vector<>(java.util.Arrays.asList(invoice.getProducts()));
        products.add(product);
        QuantProduct[] updatedProducts =
            products.toArray(new QuantProduct[0]);
        invoice.setProducts(updatedProducts);
    }

    /** Removes a product from an invoice.
     * @param invoice The invoice to update
     * @param product The product to remove
     */
    public final void removeProductFromInvoice(final Invoice invoice,
        final Product product) {
        Collection<QuantProduct> products =
            new Vector<>(java.util.Arrays.asList(invoice.getProducts()));
        products.removeIf(qp -> qp.getProduct().equals(product));
        QuantProduct[] updatedProducts =
            products.toArray(new QuantProduct[0]);
        invoice.setProducts(updatedProducts);
    }

    /** Updates the quantity of a product in an invoice.
     * @param invoice The invoice to update
     * @param product The product to update
     * @param newQuantity The new quantity
     */
    public final void updateProductQuantity(final Invoice invoice,
        final Product product, final float newQuantity) {
        QuantProduct[] products = invoice.getProducts();
        for (int i = 0; i < products.length; i++) {
            if (products[i].getProduct().equals(product)) {
                products[i] = new QuantProduct(product, newQuantity);
                break;
            }
        }
        invoice.setProducts(products);
    }

    /** Sets the buyer of an invoice.
     * @param invoice The invoice to update
     * @param buyer The new buyer
     */
    public final void setBuyer(final Invoice invoice,
        final Person buyer) {
        invoice.setBuyer(buyer);
    }

    /** Sets the seller of an invoice.
     * @param invoice The invoice to update
     * @param seller The new seller
     */
    public final void setSeller(final Invoice invoice,
        final Person seller) {
        invoice.setSeller(seller);
    }
}

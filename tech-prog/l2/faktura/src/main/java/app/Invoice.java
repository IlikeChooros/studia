package app;

import java.util.Date;

class Invoice implements DataLike {

    /** Creation date of the invoice. */
    private Date creationDate = new Date();

    /** Payment date of the invoice. */
    private Date paymentDate = new Date();

    /** Buyer of the invoice. */
    private Person buyer;

    /** Seller of the invoice. */
    private Person seller;

    /** Firm associated with the invoice. */
    private Firm firm;

    /** Products listed on the invoice. */
    private QuantProduct[] products;

    Invoice(final Date cd, final Date payment,
        final Person customer, final Person seller,
        final QuantProduct[] productList,
        final Firm firm) {
        this.creationDate = cd;
        this.paymentDate = payment;
        this.buyer = customer;
        this.seller = seller;
        this.products = productList;
        this.firm = firm;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public Date getPaymentDate() {
        return paymentDate;
    }

    public Person getBuyer() {
        return buyer;
    }

    public Person getSeller() {
        return seller;
    }

    public Firm getFirm() {
        return firm;
    }

    public QuantProduct[] getProducts() {
        return products;
    }

    public float getTotal() {
        float total = 0.0f;
        for (QuantProduct qp : products) {
            total += qp.getCumCost();
        }
        return total;
    }

    @Override
    public final String toString() {
        // Print a table-like structure
        StringBuilder sb = new StringBuilder();
        sb.append("Invoice Details:\n");
        sb.append("Creation Date: ").append(creationDate).append("\n");
        sb.append("Payment Date: ").append(paymentDate).append("\n");
        sb.append("Buyer: ").append(buyer.toString()).append("\n");
        sb.append("Seller: ").append(seller.toString()).append("\n");
        sb.append("Products:\n");
        for (QuantProduct qp : products) {
            sb.append("- ").append(qp.getProduct().toString())
                .append(", Quantity: ").append(qp.getQuantity())
                .append(", Total: ").append(
                    Formatter.formatCurrency(qp.getCumCost()))
                .append("\n");
        }
        return sb.toString();
    }
}

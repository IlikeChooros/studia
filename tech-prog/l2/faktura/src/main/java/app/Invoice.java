package app;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

class Invoice extends BaseData {

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
        final Person customer, final Person invSeller,
        final QuantProduct[] productList,
        final Firm invFirm) {
        this.creationDate = cd;
        this.paymentDate = payment;
        this.buyer = customer;
        this.seller = invSeller;
        this.products = productList;
        this.firm = invFirm;
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
                .append(", Quantity: ").append(
                    String.format("%.2f", qp.getQuantity()))
                .append(", Total: ").append(
                    Formatter.formatCurrency(qp.getCumCost()))
                .append("\n");
        }
        return sb.toString();
    }


    @Override
    public final Map<String, String> getAllFieldNames() {
        Map<String, String> fieldMap = new LinkedHashMap<>();
        fieldMap.put("creationDate", "Creation Date");
        fieldMap.put("paymentDate", "Payment Date");
        fieldMap.put("buyer", "Buyer");
        fieldMap.put("seller", "Seller");
        fieldMap.put("firm", "Firm");
        fieldMap.put("products", "Products");
        return fieldMap;
    }

    @Override
    public final boolean updateField(final String field,
        final Object... values) {
        if (values.length == 0) {
            return false;
        }

        switch (field) {
            case "creationDate":
                if (values[0].getClass() != Date.class) {
                    return false;
                }
                this.creationDate = (Date) values[0];
                return true;
            case "paymentDate":
                if (values[0].getClass() != Date.class) {
                    return false;
                }
                this.paymentDate = (Date) values[0];
                return true;
            case "buyer":
                if (values[0].getClass() != Person.class) {
                    return false;
                }
                this.buyer = (Person) values[0];
                return true;
            case "seller":
                if (values[0].getClass() != Person.class) {
                    return false;
                }
                this.seller = (Person) values[0];
                return true;
            case "firm":
                if (values[0].getClass() != Firm.class) {
                    return false;
                }
                this.firm = (Firm) values[0];
                return true;
            case "products":
                if (values[0].getClass() != QuantProduct[].class) {
                    return false;
                }
                this.products = (QuantProduct[]) values[0];
                return true;
            default:
                return false;
        }
    }
}

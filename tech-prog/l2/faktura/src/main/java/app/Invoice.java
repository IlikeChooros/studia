package app;

import java.util.Date;

class Invoice implements DataLike {
    public Date CreationDate = new Date();
    public Date PaymentDate = new Date();
    public Person Buyer;
    public Person Seller;
    public QuantProduct[] Products;

    public Invoice(Date creationDate, Date paymentDate, Person buyer, Person seller, QuantProduct[] products) {
        this.CreationDate = creationDate;
        this.PaymentDate = paymentDate;
        this.Buyer = buyer;
        this.Seller = seller;
        this.Products = products;
    }

    // enum FieldName {
    //     PURCHASE_DATE,
    //     PAYMENT_DATE,
    //     BUYER,
    //     SELLER,
    //     PRODUCTS;

    //     @Override
    //     public String toString() {
    //         switch (this) {
    //             case PURCHASE_DATE:
    //                 return "Data Wystawienia";
    //             case PAYMENT_DATE:
    //                 return "Data Platności";
    //             case BUYER:
    //                 return "Kupujący";
    //             case SELLER:
    //                 return "Sprzedawca";
    //             case PRODUCTS:
    //                 return "Produkty";
    //             default:
    //                 return "";
    //         }
    //     }
    // }

    // public String[] GetAllFieldNames() {
        // return new String[]{"Data Wystawienia", "Data Platności", "Kupujący", "Sprzedawca", "Produkty"};
    // }

    // public void UpdateField(String fieldName, Object... values) {
    //     switch (fieldName) {
    //         case "Data Wystawienia":
    //             this.CreationDate = (Date) values[0];
    //             break;
    //         case "Data Platności":
    //             this.PaymentDate = (Date) values[0];
    //             break;
    //         case "Kupujący":
    //             this.Buyer = (Person) values[0];
    //             break;
    //         case "Sprzedawca":
    //             this.Seller = (Person) values[0];
    //             break;
    //         case "Produkty":
    //             this.Products[(int)values[0]].UpdateField((String)values[1], values[2]);
    //             break;
    //     }
    // }
}



package app;

public class QuantProduct implements DataLike {
    public Product Product;
    public int Quantity;

    public QuantProduct(Product prod, int quantity) {
        this.Product = prod;
        this.Quantity = quantity;
    }

    // Cumulative sum of costs :))))
    public float Cum() {
        return Quantity * Product.Cost;
    }

    // @Override
    // public String[] GetAllFieldNames() {
    //     return new String[]{"Nazwa", "Koszt", "Ilość"};
    // }

    // @Override
    // public void UpdateField(String field, Object... values) {
    //     switch (field) {
    //         case "Nazwa":
    //             this.Product.Name = (String) values[0];
    //             break;
    //         case "Koszt":
    //             this.Product.Cost = (Float) values[0];
    //             break;
    //         case "Ilość":
    //             this.Quantity = (int) values[0];
    //             break;
    //     }
    // }
}

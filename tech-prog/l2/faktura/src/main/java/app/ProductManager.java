package app;

import java.util.Vector;

public class ProductManager extends ManagerHelper<Product> {
    ProductManager(final Vector<Product> coll) {
        super(coll);
    }
}

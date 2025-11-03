package app;

public class Artykuly {
    public Produkt Prod;
    public int Ilosc;

    // Cumulative sum of costs :))))
    public float Cum() {
        return Ilosc * Prod.Koszt;
    }
}

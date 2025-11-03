package app;

import java.util.Date;

public class Manager {
    private Faktura faktura = new Faktura();

    // Print to stdout
    public void Display() {

    }

    public void SaveToPdf() {
        
    }

    public void SetDaneKupujacego(DaneKupujacego kup) {
        faktura.Kupujacy = kup;
    }

    public void SetDaneSprzedajacego(DaneSprzedawcy sprzed) {
        faktura.Sprzedawca = sprzed;
    }

    public void SetPaymentDate(Date date) {
        faktura.DataPlatonsci = date;
    }

    public void SetCreationDate(Date date) {
        faktura.DataWystawienia = date;
    }
}

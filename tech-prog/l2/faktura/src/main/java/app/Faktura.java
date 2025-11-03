package app;

import java.util.Date;

class Faktura {
    public Date DataWystawienia = new Date();
    public Date DataPlatonsci = new Date();
    public DaneSprzedawcy Sprzedawca;
    public DaneKupujacego Kupujacy;
    public Artykuly[] Artykulies;

    public String[] GetAllFieldNames() {
        return null;
    }

    public void UpdateField(String fieldName, String value) {
        
    }
}



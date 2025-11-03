package app;

public class DaneSprzedawcy implements DataLike {
    public String Nazwafirmy;
    public String DaneKontaktowe;

    public enum Fields {
        NAZWA_FIRMY("Nazwa firmy"),
        DANE_KONTATKOWE("Dane kontatkowe");
        

        private String name;
        Fields(String name) {
            this.name = name;
        }

        public String getName() {
            return this.name;
        }
    }

    public String[] GetAllFieldNames() {
        return new String[]{"Nazwa firmy", "Dane kontaktowe"};
    }

    public void UpdateField(String field, String v) {
        for()
    }
}

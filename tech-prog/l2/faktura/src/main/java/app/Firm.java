package app;

/**
 * Represents a company/firm with name, address and tax identification (NIP).
 */
public class Firm extends BaseData {
    /** Company name. */
    private String name;
    /** Company address. */
    private String address;
    /** Tax identification number (NIP). */
    private String nip;

    /**
     * Create a new Firm.
     *
     * @param firmName the company name
     * @param firmAddress the company address
     * @param taxID the tax identification number (NIP)
     */
    public Firm(final String firmName,
        final String firmAddress, final String taxID) {
        this.name = firmName;
        this.address = firmAddress;
        this.nip = taxID;
    }

    /**
     * Get the company name.
     *
     * @return the company name
     */
    public String getName() {
        return name;
    }

    /**
     * Get the company address.
     *
     * @return the company address
     */
    public String getAddress() {
        return address;
    }

    /**
     * Get the company's identification number (NIP).
     *
     * @return the NIP (id number) of the company
     */
    public String getNIP() {
        return nip;
    }

    @Override
    public final String toString() {
        return name + ", Address: " + address + ", NIP: " + nip;
    }

    @Override
    public final java.util.Map<String, String> getAllFieldNames() {
        java.util.Map<String, String> fieldNames = new java.util.HashMap<>();
        fieldNames.put("name", "Company Name");
        fieldNames.put("address", "Company Address");
        fieldNames.put("nip", "Tax Identification Number (NIP)");
        return fieldNames;
    }

    @Override
    public final boolean updateField(
        final String field, final Object... values) {
        if (values.length != 1 || values[0].getClass() != String.class) {
            return false;
        }

        if (field.equalsIgnoreCase("name")) {
            this.name = (String) values[0];
            return true;
        } else if (field.equalsIgnoreCase("address")) {
            this.address = (String) values[0];
            return true;
        } else if (field.equalsIgnoreCase("nip")) {
            this.nip = (String) values[0];
            return true;
        }
        return false;
    }

    @Override
    public final Object getValue(final String fieldName) {
        switch (fieldName.toLowerCase()) {
            case "name":
                return name;
            case "address":
                return address;
            case "nip":
                return nip;
            default:
                return null;
        }
    }
}

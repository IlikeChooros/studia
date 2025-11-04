package app;

/**
 * Represents a company/firm with name, address and tax identification (NIP).
 */
public class Firm implements DataLike {
    /** Company name. */
    private String name;
    /** Company address. */
    private String address;
    /** Tax identification number (NIP). */
    private String nip;

    /**
     * Create a new Firm.
     *
     * @param name the company name
     * @param address the company address
     * @param nip the tax identification number (NIP)
     */
    public Firm(final String name, final String address, final String nip) {
        this.name = name;
        this.address = address;
        this.nip = nip;
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
}

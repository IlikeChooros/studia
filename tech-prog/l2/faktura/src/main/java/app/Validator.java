package app;

public final class Validator {

    private Validator() {
        // Prevent instantiation
    }

    /**
     * Validates a NIP (Polish Tax Identification Number).
     * @param nip
     * @return true if valid
     */
    public static boolean isValidNIP(final String nip) {
        return nip != null && nip.matches("\\d{10}");
    }

    /**
     * Validates an ID Number.
     * @param idNumber
     * @return true if valid
     */
    public static boolean isValidIDNumber(final String idNumber) {
        // Simple ID Number validation: 9 digits
        return idNumber != null && idNumber.matches("\\d{9}");
    }

    /**
     * Validates a PESEL (Polish National Identification Number).
     * @param pesel
     * @return true if valid
     */
    public static boolean isValidPESEL(final String pesel) {
        // Simple PESEL validation: 11 digits
        return pesel != null && pesel.matches("\\d{11}");
    }

    /**
     * Validates if the given string is a valid non-negative float.
     *
     * @param price the string to validate
     * @return true if the string represents a valid non-negative float
     */
    public static boolean isValidFloat(final String price) {
        try {
            float p = Float.parseFloat(price);
            return p >= 0;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * Validates if the given string is a valid non-negative integer.
     *
     * @param quantity the string to validate
     * @return true if the string represents a valid non-negative integer
     */
    public static boolean isValidInt(final String quantity) {
        try {
            int q = Integer.parseInt(quantity);
            return q >= 0;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public enum Units {
        /** Unit: pieces. */
        SZT,
        /** Unit: kilograms. */
        KG,
        /** Unit: liters. */
        L,
        /** Unit: meters. */
        M,
        /** Unit: centimeters. */
        CM,
        /** Unit: millimeters. */
        MM;

        @Override
        public String toString() {
            switch (this) {
                case SZT:
                    return "szt";
                case KG:
                    return "kg";
                case L:
                    return "l";
                case M:
                    return "m";
                case CM:
                    return "cm";
                case MM:
                    return "mm";
                default:
                    return "";
            }
        }
    }

    /**
     * Retrieves all valid units as a comma-separated string.
     *
     * @return a string containing all valid units
     */
    public static String getAllUnits() {
        StringBuilder sb = new StringBuilder();
        int count = 0;
        int len = Units.values().length;
        for (Units u : Units.values()) {
            sb.append(u.toString());
            if (count < len - 1) {
                sb.append(", ");
            }
            count++;
        }
        return sb.toString();
    }

    /**
     * Validates if the given unit string is a valid unit.
     *
     * @param unit the unit string to validate
     * @return true if the unit is valid, false otherwise
     */
    public static boolean isValidUnit(final String unit) {
        for (Units u : Units.values()) {
            if (u.toString().equals(unit)) {
                return true;
            }
        }
        return false;
    }
}

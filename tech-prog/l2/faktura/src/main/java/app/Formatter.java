package app;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public final class Formatter {

    private Formatter() {
        // Prevent instantiation
    }

    /**
     * Format a float amount as currency in PLN.
     *
     * @param amount the amount to format
     * @return the formatted currency string
     */
    public static String formatCurrency(final float amount) {
        return String.format("%.2f PLN", amount);
    }

    /**
     * Format a quantity with its unit.
     *
     * @param quantity the quantity to format
     * @param unit the unit of measure
     * @return the formatted quantity string
     */
    public static String formatQuantity(
        final float quantity, final String unit) {
        return String.format("%.2f %s", quantity, unit);
    }

    /**
     * Format a Date object as a string in the format YYYY-MM-DD.
     *
     * @param date the date to format
     * @return the formatted date string
     */
    public static String formatDate(final Date date) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        return sdf.format(date);
    }


    /**
     * Parse a date string in the format YYYY-MM-DD to a Date object.
     *
     * @param dateStr the date string to parse
     * @return the parsed Date object
     * @throws ParseException if the date string is invalid
     */
    public static Date parseDate(final String dateStr) throws ParseException {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        return sdf.parse(dateStr);
    }
}

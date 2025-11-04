package app;


public final class Messenger {
    /**
     * Input prompt format.
     */
    private static final String INPUT_FMT = "\033[38;5;183m[?] %s\033[0m >>> ";

    /**
     * Error message format.
     */
    private static final String ERROR_FMT = "\033[1;31m[-] %s\033[0m\n";

    /**
     * Info message format.
     */
    private static final String INFO_FMT = "\033[1;34m[*] %s\033[0m\n";

    /**
     * Help message format.
     */
    private static final String HELP_FMT = "\033[1;36m[H] %s\033[0m\n";

    /**
     * Data message format.
     */
    private static final String DATA_FMT = "\033[1;33m[>]\t%s\033[0m\n";

    /**
     * Success message format.
     */
    private static final String SUCCESS_FMT = "\033[1;32m[+] %s\033[0m\n";

    private Messenger() {
        throw new AssertionError("Utility class should not be instantiated");
    }

    /**
     * Print an input prompt message.
     *
     * @param msg the message to print
     */
    public static void input(final String msg) {
        System.out.printf(INPUT_FMT, msg);
    }

    /**
     * Print an error message.
     *
     * @param msg the message to print
     */
    public static void error(final String msg) {
        System.out.printf(ERROR_FMT, msg);
    }

    /**
     * Print a data message.
     *
     * @param msg the message to print
     */
    public static void data(final String msg) {
        System.out.printf(DATA_FMT, msg);
    }

    /**
     * Print an info message.
     *
     * @param msg the message to print
     */
    public static void info(final String msg) {
        System.out.printf(INFO_FMT, msg);
    }

    /**
     * Print a help message.
     *
     * @param msg the message to print
     */
    public static void help(final String msg) {
        System.out.printf(HELP_FMT, msg);
    }

    /**
     * Print a success message.
     *
     * @param msg the message to print
     */
    public static void success(final String msg) {
        System.out.printf(SUCCESS_FMT, msg);
    }
}

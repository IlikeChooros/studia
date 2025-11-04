package app;

import org.jline.terminal.Terminal;

public final class Messenger {

    /** The terminal to use for input/output. */
    private Terminal terminal;

    /**
     * Create a new Messenger.
     *
     * @param term the terminal to use for input/output
     */
    public Messenger(final Terminal term) {
        this.terminal = term;
    }

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

    /**
     * Format an input prompt message.
     *
     * @param msg the message to format
     * @return the formatted message
     */
    public static String fmtInput(final String msg) {
        return String.format(INPUT_FMT, msg);
    }

    /**
     * Format an error message.
     *
     * @param msg the message to format
     * @return the formatted message
     */
    public static String fmtError(final String msg) {
        return String.format(ERROR_FMT, msg);
    }

    /**
     * Format a data message.
     *
     * @param msg the message to format
     * @return the formatted message
     */
    public static String fmtData(final String msg) {
        return String.format(DATA_FMT, msg);
    }

    /**
     * Format an info message.
     *
     * @param msg the message to format
     * @return the formatted message
     */
    public static String fmtInfo(final String msg) {
        return String.format(INFO_FMT, msg);
    }

    /**
     * Format a help message.
     *
     * @param msg the message to format
     * @return the formatted message
     */
    public static String fmtHelp(final String msg) {
        return String.format(HELP_FMT, msg);
    }

    /**
     * Format a success message.
     *
     * @param msg the message to format
     * @return the formatted message
     */
    public static String fmtSuccess(final String msg) {
        return String.format(SUCCESS_FMT, msg);
    }

    /**
     * Print an input prompt message.
     *
     * @param msg the message to print
     */
    public void input(final String msg) {
        terminal.writer().printf(INPUT_FMT, msg);
    }

    /**
     * Print an error message.
     *
     * @param msg the message to print
     */
    public void error(final String msg) {
        terminal.writer().printf(ERROR_FMT, msg);
    }

    /**
     * Print a data message.
     *
     * @param msg the message to print
     */
    public void data(final String msg) {
        terminal.writer().printf(DATA_FMT, msg);
    }

    /**
     * Print an info message.
     *
     * @param msg the message to print
     */
    public void info(final String msg) {
        terminal.writer().printf(INFO_FMT, msg);
    }

    /**
     * Print a help message.
     *
     * @param msg the message to print
     */
    public void help(final String msg) {
        terminal.writer().printf(HELP_FMT, msg);
    }

    /**
     * Print a success message.
     *
     * @param msg the message to print
     */
    public void success(final String msg) {
        terminal.writer().printf(SUCCESS_FMT, msg);
    }
}

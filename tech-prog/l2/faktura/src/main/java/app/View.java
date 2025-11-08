package app;

import java.util.Vector;
import java.util.function.Function;

import org.jline.reader.LineReader;

public class View {
    /** The line reader for user input. */
    protected LineReader reader;

    /** The messenger for formatted messages. */
    protected Messenger messenger;

    /** The context-aware completer for command suggestions. */
    protected CommandHelper.ContextAwareCompleter contextCompleter;

    View(final Messenger mess,
        final CommandHelper.ContextAwareCompleter completer,
        final LineReader lineReader
    ) {
        this.messenger = mess;
        this.contextCompleter = completer;
        this.reader = lineReader;
    }

    protected final String readInput(final String prompt) {
        return reader.readLine(prompt).trim();
    }

    /** Prompts the user for input with validation and default value support.
     *
     * @param msg          The prompt message to display.
     * @param f            A function to validate the input.
     * If null, no validation
     *                     is performed.
     * @param errmsg       The error message to display if validation fails.
     * @param defaultValue The default value to return
     * if the user provides no input.
     *                     If null, no default is used.
     * @return The validated user input or the default value.
     */
    public String prompt(final String msg,
        final Function<String, Boolean> f, final String errmsg,
        final String defaultValue) {

        while (true) {
            String input = readInput(Messenger.fmtInput(msg));

            if (input.isEmpty() && defaultValue != null) {
                return defaultValue;
            }

            if (f == null) {
                return input;
            }

            if (f.apply(input)) {
                return input;
            } else {
                messenger.error(errmsg);
            }
        }
    }

    /** Prompts the user to select an item from a list based on search input.
     *
     * @param msg      The prompt message to display.
     * @param searchFn A function that takes a search string
     * and returns a vector of matching items.
     * @param indexFn  A function that takes an index
     * and returns the corresponding item.
     * @param defaultValue The default value to return
     * if the user provides no input.
     *                     If null, no default is used.
     * @return The selected item or null if no selection was made.
     */
    public Object select(
        final String msg,
        final Function<String, Vector<?>> searchFn,
        final Function<Integer, ?> indexFn,
        final String defaultValue) {
        // If the user specifies an index, return that person
        // else perform a search by name and make
        // a suggestion for the first match
        // other matches added as tab completions
        reader.setVariable(CommandHelper.COMPLETER_CONTEXT_VAR, "select");
        contextCompleter.setContextFilter(searchFn);

        while (true) {
            String data = prompt(msg, null, null, defaultValue).trim();

            if (CommandHelper.isExitCommand(data)) {
                return null;
            }

            Vector<?> matches = searchFn.apply(data);
            if (matches.isEmpty()) {
                messenger.info("No matches found for: " + data);
                continue;
            }

            if (matches.size() == 1) {
                return matches.get(0);
            } else {
                messenger.info(String.format(
                    "%d matches found", matches.size()));
            }
        }
    }

    /** Prompts the user to select an item from a manager.
     *
     * @param msg The prompt message to display.
     * @param man The manager to select from.
     * @param <T> The type of data managed.
     * @return The selected item or null if no selection was made.
     */
    @SuppressWarnings("unchecked")
    public <T extends DataLike> T
    defaultSelect(final String msg, final ManagerLike<T> man) {
        return (T) select(msg, man::search, man::getById, null);
    }
}

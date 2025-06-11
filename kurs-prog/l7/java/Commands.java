import java.util.Arrays;
import java.util.Vector;

class ValidationError extends Exception {
    public ValidationError(String reason) {
        super(reason);
    }
}

class Option<E> {

    @FunctionalInterface
    public static interface Callback<E> {
        public void apply(E value);
    }

    @FunctionalInterface
    public static interface Validator {
        void validate(String[] args) throws ValidationError;
    }

    @FunctionalInterface
    public static interface Setter<E> {
        E convert(String[] toConvert);
    }

    private String[] names = null;
    private String desctription = null;
    private Callback<E> callback = null;
    private Validator validator = null;
    private Setter<E> setter = null;
    private E value = null;
    
    public static Validator defaultValidator() {
        return new Validator() {
            @Override
            public void validate(String[] args) throws ValidationError {
                return;
            }
        };
    }

    public Option(String[] names, String description, Callback<E> callback, Validator validator, Setter<E> setter) {
        this.names = names;
        Arrays.sort(this.names);
        this.callback = callback;
        this.validator = validator;
        this.setter = setter;
        this.desctription = description;

        if (validator == null) {
            this.validator = defaultValidator();
        }
    }

    public E get() {
        return this.value;
    }

    public void parse(String[] values) throws ValidationError {
        // Will throw the error if failed
        validator.validate(values);

        // Data is ok
        value = setter.convert(values);

        // Invoke the callback
        if (callback != null) {
            callback.apply(value);
        }
    }

    public boolean is(String name) {
        return Arrays.binarySearch(names, name) >= 0;
    }

    public String[] flags() {
        return this.names;
    }

    public String desc() {
        return this.desctription;
    }
};

public class Commands {
    private final Vector<Option<?>> options = new Vector<>();
    private Option.Callback<String> helpCallback = null;

    private String[] PREFIX_COMMANDS = {"/", "--"};
    private final String MAX_COMMAND = "max";
    private final String MIN_COMMAND = "min";
    private final String PRINT_COMMAND = "print";
    private final String ADD_COMMAND = "add";
    private final String DELETE_COMMAND = "delete";
    private final String SEARCH_COMMAND = "search";

    public static Option.Setter<Boolean> booleanSetter() {
        return (args) -> {
            return true;
        };
    }

    public static String stringSetter(String[] s) throws ValidationError {
        return s.toString();
    }

    public static Integer ingtegerSetter(String s) throws ValidationError {
        try {
            return Integer.parseInt(s);
        }
        catch (NumberFormatException exception) {
            throw new ValidationError(s + " is not a number");
        }
    }

    public static Double doubleSetter(String s) throws ValidationError {
        try {
            return Double.parseDouble(s);
        } catch (NumberFormatException e) {
            throw new ValidationError(s + " is not a floating point number");
        }
    }

    public Commands(Option.Callback<String> helpCallback) {
        this.helpCallback = helpCallback;

        if (helpCallback == null) {
            // Just print to the output stream
            this.helpCallback = (help) -> {
                System.out.println(help);
            };
        }
    }


    /**
     * Get help formatted help message as a String
     */
    public String help() {
        StringBuilder builder = new StringBuilder();
        
        builder.append("Options: \n");
        for (Option<?> option : options) {

            builder.append("   ");
            for (String flag : option.flags()) {
                builder.append(flag).append(" ");
            }

            builder.append("\n  ").append(option.desc());
            builder.append("\n");
        }

        return builder.toString();
    }

    @Override
    public String toString() {
        return help();
    }

    /**
     * Set custom long and short prefixes
     */
    public void setPrefixes(String longPrefix, String shortPrefix) {
        this.PREFIX_COMMANDS[0] = shortPrefix;
        this.PREFIX_COMMANDS[1] = longPrefix;
    }
    
    /**
     * Add new command with given type
     */
    public <E> void add(
        String[] names, String desctription, Option.Callback<E> callback, 
        Option.Validator validator, Option.Setter<E> setter
    ) {
        options.add(new Option<E>(
            names, desctription, callback, validator, setter
        ));
    }

    /**
     * Add new option to the commands
     */
    public <E> void add(Option<E> option) {
        this.options.add(option);
    }

    /**
     * Parse the arguments, and call the callbacks
     */
    public void parse(String[] args) throws ValidationError {

        if (args == null) {
            return;
        }

        // Parse the arguments, look for the flags
        Vector<String> values = new Vector<>();
        int index = 0;
        boolean usedIncorrectly = true;

        while (args.length > index) {
            if (args[index].startsWith(PREFIX_COMMANDS[0]) || 
                args[index].startsWith(PREFIX_COMMANDS[1])) {

                usedIncorrectly = false;

                // Check if that's a 'help' command
                if (args[index].equals(PREFIX_COMMANDS[0] + "help")) {
                    helpCallback.apply(help());
                    break;
                }

                // Find the option
                Option<?> option = null;

                for (int i = 0; i < options.size(); i++) {
                    if (options.get(i).is(args[index])) {
                        option = options.get(index);
                        break;
                    }
                }

                index++;

                // Check if we found it
                if (option == null) {
                    continue;
                }

                // Parse the arguments
                while (index < args.length && !args[index].startsWith("-")) {
                    values.add(args[index]);
                    index++;
                }

                // Call the option to parse these values
                // Might throw ValidationError
                option.parse((String[])values.toArray());
            }
            else {
                index++;
            }            
        }

        // Check if we didn't get any valid command
        if (usedIncorrectly) {
            StringBuilder builder = new StringBuilder();
            for (String arg : args) {
                builder.append(arg).append(" ");
            }
            helpCallback.apply(
                String.format(
                    "Invalid command: %s\n Type %shelp for a list of all commands", 
                    builder.toString(), PREFIX_COMMANDS[0]
            ));
        } 
    }
}

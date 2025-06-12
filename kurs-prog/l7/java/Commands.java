import java.util.Arrays;
import java.util.Vector;

class ValidationException extends Exception {
    public ValidationException(String reason) {
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
        void validate(String[] args) throws ValidationException;
    }

    @FunctionalInterface
    public static interface Converter<E> {
        E convert(String[] toConvert);
    }

    private String[] names = null;
    private String desctription = null;
    private Callback<E> callback = null;
    private Validator validator = null;
    private Converter<E> converter = null;
    private E value = null;
    
    /**
     * Create a default validator - one that allows every input
     */
    public static Validator defaultValidator() {
        return new Validator() {
            @Override
            public void validate(String[] args) throws ValidationException {
                return;
            }
        };
    }

    /**
     * Create a boolean converter, simply return true
     * @return
     */
    public static Converter<Boolean> booleanConverter() {
        return (args) -> {
            return true;
        };
    }

    public Option(String[] names, String description, Callback<E> callback, Validator validator, Converter<E> converter) {
        this.names = names;
        Arrays.sort(this.names);
        this.callback = callback;
        this.validator = validator;
        this.converter = converter;
        this.desctription = description;

        if (validator == null) {
            this.validator = defaultValidator();
        }
    }

    public E get() {
        return this.value;
    }

    public void parse(String[] values) throws ValidationException {
        // Will throw the error if failed
        validator.validate(values);

        // Data is ok
        value = converter.convert(values);

        // Invoke the callback
        if (callback != null) {
            callback.apply(value);
        }
    }

    public boolean is(String name) {
        for (String flag : names) {
            if (flag.equals(name)) {
                System.out.println(flag + " == " + name);
                return true;
            }
        }
        
        return false;
        // return Arrays.binarySearch(names, name) >= 0;
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
        Option.Validator validator, Option.Converter<E> converter
    ) {
        options.add(new Option<E>(
            names, desctription, callback, validator, converter
        ));
    }

    /**
     * Add new option to the commands
     */
    public <E> void add(Option<E> option) {
        this.options.add(option);
    }

    private boolean isCommand(String argument) {
        return argument == null || argument.startsWith(PREFIX_COMMANDS[0]) ||
                argument.startsWith(PREFIX_COMMANDS[1]);
    }

    /**
     * Parse the arguments, and call the callbacks
     */
    public void parse(String[] args) throws ValidationException {

        if (args == null) {
            return;
        }

        // Parse the arguments, look for the flags
        Vector<String> values = new Vector<>();
        int index = 0;
        boolean usedIncorrectly = true;

        while (args.length > index) {
            if (isCommand(args[index])) {

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
                        option = options.get(i);
                        break;
                    }
                }

                index++;

                // Check if we found it
                if (option == null) {
                    continue;
                }

                // Parse the arguments
                values.clear();
                while (index < args.length && !isCommand(args[index])) {
                    values.add(args[index]);
                    index++;
                }

                // Call the option to parse these values
                // Might throw ValidationException
                String[] optionArgs = null;

                if (!values.isEmpty()) {
                    optionArgs = values.toArray(new String[values.size()]);
                }
                
                option.parse(optionArgs);
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

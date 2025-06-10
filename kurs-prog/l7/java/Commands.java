public class Commands {
    private final String MAX_COMMAND = "max";
    private final String MIN_COMMAND = "min";
    private final String PRINT_COMMAND = "print";
    private final String ADD_COMMAND = "add";
    private final String DELETE_COMMAND = "delete";
    private final String SEARCH_COMMAND = "search";

    public static String stringSetter(String s) {
        return s;
    }

    public static Integer ingtegerSetter(String s) {
        try {
            return Integer.parseInt(s);
        }
        catch (NumberFormatException exception) {
            return null;
        }
    }

    public static Double doubleSetter(String s) {
        try {
            return Double.parseDouble(s);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    
}

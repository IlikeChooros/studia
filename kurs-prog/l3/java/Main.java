
public class Main {

    enum FigureTypes {
        OKRAG,
        PIECIOKAT,
        SZESCIOKAT,
        CZWOROKAT
    }

    public static int getArgCountForType(FigureTypes type) {
        if (type == FigureTypes.CZWOROKAT){
            return 5;
        }
        return 1;
    }

    public static FigureTypes getFigureType(char figureType) {
        for (FigureTypes type : FigureTypes.values()) {
            if (type.toString().toLowerCase().charAt(0) == figureType) {
                return type;
            }
        }

        throw new IllegalArgumentException("Niepoprawny typ figury: " + figureType);
    }

    public static boolean isValidFigureType(char figureType) {
        try{
            getFigureType(figureType);
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    public static Figures.FigureTypeOneParam getFigure1(int param, FigureTypes type) {
        switch (type) {
            case OKRAG:
                return Figures.FigureTypeOneParam.CIRCLE;
            case PIECIOKAT:
                return Figures.FigureTypeOneParam.REGULAR_PENTAGON;
            case SZESCIOKAT:
                return Figures.FigureTypeOneParam.REGULAR_HEXAGON;
            case CZWOROKAT:
            default:
                return Figures.FigureTypeOneParam.SQUARE;
        }
    }

    public static int parseArgs(String[] args, int start) {
        String token = args[start];
        int end = start + 1;

        if (token.length() != 1 || !isValidFigureType(token.charAt(0)) ) {
            throw new IllegalArgumentException("Niepoprawny typ figury: " + token);
        }

        FigureTypes type = getFigureType(token.charAt(0));
        int argCount = getArgCountForType(type);
        int[] params = new int[5];

        for(int i = 0; i < argCount; i++, end++) {
            int parsedValue;
            try {
                parsedValue = Integer.parseInt(args[end]);
            }
            catch (NumberFormatException e) {
                throw new IllegalArgumentException("Niepoprawna wartość argumentu: " + args[end]);
            }

            params[i] = parsedValue;
        }

        if ()

        return end;
    }


    public static void main(String[] args) {
        if (args.length <= 1) {
            System.out.println("Podaj co najmniej 2 argumenty.");
            return;
        }
    }
}

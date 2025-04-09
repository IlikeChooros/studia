package z1;

public class Main {

    enum FigureTypes {
        OKRAG,
        PIECIOKAT,
        SZESCIOKAT,
        CZWOROKAT
    }

    enum AllFigureTypes {
        OKRAG,
        PIECIOKAT,
        SZESCIOKAT,
        KWADRAT,
        PROSTOKAT,
        ROMB,
        NONE
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

    public static boolean isOneParamTypeFigure(AllFigureTypes type) {
        return type == AllFigureTypes.OKRAG || 
               type == AllFigureTypes.PIECIOKAT || 
               type == AllFigureTypes.SZESCIOKAT ||
                type == AllFigureTypes.KWADRAT;
    }

    public static AllFigureTypes getAllFigureType(FigureTypes type, int[] params) {
        switch (type) {
            case OKRAG:
                return AllFigureTypes.OKRAG;
            case PIECIOKAT:
                return AllFigureTypes.PIECIOKAT;
            case SZESCIOKAT:
                return AllFigureTypes.SZESCIOKAT;
            case CZWOROKAT:
            default:
                // Either rectangle, square or rhombus
                if (params[0] == params[2] && 
                    params[1] == params[3]) 
                {
                    if (params[4] == 90) // square or rectangle
                    {
                        if (params[0] == params[1]) {
                            return AllFigureTypes.KWADRAT;
                        } 
                        return AllFigureTypes.PROSTOKAT;
                    }

                    // else, check if that's a rhombus
                    if (params[0] == params[1]) {
                        return AllFigureTypes.ROMB;
                    }
                }
                return AllFigureTypes.NONE;
        }
    }

    public static int findNextTypeToken(String[] args, int start) {
        for (int i = start; i < args.length; i++) {
            if (args[i].length() == 1 && isValidFigureType(args[i].charAt(0))) {
                return i;
            }
        }
        return args.length;
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

            if (parsedValue <= 0) {
                throw new IllegalArgumentException("Ujemna wartość argumentu: " + args[end]);
            }

            params[i] = parsedValue;
        }

        AllFigureTypes alltype = getAllFigureType(type, params);

        if (alltype == AllFigureTypes.NONE) {
            throw new IllegalArgumentException("Niepoprawny typ figury");
        }
        

        return end;
    }


    public static void main(String[] args) {
        if (args.length <= 1) {
            System.out.println("Podaj co najmniej 2 argumenty.");
            return;
        }

        int i = 0;

        while (i < args.length) {
            try {
                i = parseArgs(args, i);
            } catch (IllegalArgumentException e) {
                System.out.println(e.getMessage());
                i++;
                
                // Find next type token
                i = findNextTypeToken(args, i);
            }
        }
    }
}


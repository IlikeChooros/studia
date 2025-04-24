
public class Main {

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

    public static Figure getFigure(AllFigureTypes type, int[] params) {
        switch (type) {
            case KWADRAT:
                return new Square(params[0]);
            case OKRAG:
                return new Circle(params[0]);
            case PIECIOKAT:
                return new Pentagon(params[0]);
            case SZESCIOKAT:
                return new Hexagon(params[0]);
            case ROMB:
                return new Rhombus(params[0], params[4]);
            case PROSTOKAT:
            default:
                return new Rectangle(params[0], params[1]);
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

    public static int getNumberOfFigures(String[] args) {
        int count = 0;
        for(String s : args) {
            if (s.length() == 1 && isValidFigureType(s.charAt(0))) {
                count++;
            }
        }
        return count;
    }

    public static void main(String[] args) {
        if (args.length <= 1) {
            System.out.println("Podaj co najmniej 2 argumenty.");
            return;
        }

        int figure_count = getNumberOfFigures(args);

        if (figure_count == 0) {
            System.out.println("Podaj typ figury i parametry.");
            return;
        }

        Figure[] figures = new Figure[figure_count];

        int start = 0;
        int figure_index = 0;
        while (start < args.length) {
            try {
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
                
                figures[figure_index] = getFigure(alltype, params);
                start = end;
                figure_index++;

            } catch (IllegalArgumentException e) {
                System.out.println(e.getMessage());
                start++;
                
                // Find next type token
                start = findNextTypeToken(args, start);
            }
        }

        for(int j = 0; j < figure_index; j++) {
            System.out.println(
                figures[j].getName() + " --> pole: " +
                figures[j].getArea() + ", obwód: " + 
                figures[j].getPerimeter()
            );
        }
    }
}


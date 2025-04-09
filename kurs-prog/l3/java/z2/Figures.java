package z2;

interface Figure1Param {
    double getArea(double lenght);
    double getPerimeter(double lenght);
    String getName();
}

interface Figure2Param {
    double getArea(double a, double b);
    double getPerimeter(double a, double b);
    String getName();
}

public class Figures {

    public static double calculateRegularPolygonArea(int n, double a) {
        return (n * Math.pow(a, 2)) / (4 * Math.tan(Math.PI / n));
    }

    public enum FigureTypeTwoParam implements Figure2Param {
        RECTANGLE {
            @Override
            public String getName() {
                return "Prostokąt";
            }

            @Override
            public double getArea(double a, double b) {
                return a * b;
            }

            @Override
            public double getPerimeter(double a, double b) {
                return 2 * (a + b);
            }
        },
        RHOMBUS {
            @Override
            public String getName() {
                return "Romb";
            }

            @Override
            public double getArea(double side, double angle) {
                return side * side * Math.sin(Math.toRadians(angle));
            }

            @Override
            public double getPerimeter(double a, double b) {
                return 4 * a;
            }
        }
    }


    public enum FigureTypeOneParam implements Figure1Param {
        CIRCLE {
            @Override
            public String getName() {
                return "Koło";
            }

            @Override
            public double getArea(double a) {
                return Math.PI * Math.pow(a, 2);
            }

            @Override
            public double getPerimeter(double a) {
                return 2 * Math.PI * a;
            }
        },
        SQUARE {
            @Override
            public String getName() {
                return "Kwadrat";
            }

            @Override
            public double getArea(double a) {
                return Math.pow(a, 2);
            }

            @Override
            public double getPerimeter(double a) {
                return 4 * a;
            }
        },
        REGULAR_PENTAGON {
            @Override
            public String getName() {
                return "Pięciokąt foremny";
            }

            @Override
            public double getArea(double a) {
                // return (Math.sqrt(5 * (5 + 2 * Math.sqrt(5))) / 4) * Math.pow(a, 2);
                return calculateRegularPolygonArea(5, a);
            }

            @Override
            public double getPerimeter(double a) {
                return 5 * a;
            }
        },
        REGULAR_HEXAGON {
            @Override
            public String getName() {
                return "Sześciokąt foremny";
            }

            @Override
            public double getArea(double a) {
                // return (3 * Math.sqrt(3) / 2) * Math.pow(a, 2);
                return calculateRegularPolygonArea(6, a);
            }

            @Override
            public double getPerimeter(double a) {
                return 6 * a;
            }
        };
    }
    
}
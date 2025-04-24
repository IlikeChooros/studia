
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

interface Calculations {
    public double getArea();
    public double getPerimeter();
}

abstract class Figure implements Calculations {
    public String getName() {
        return "Figura";
    }
}

abstract class FourSided extends Figure {
    protected double a, b, angle;

    public FourSided(double a, double b, double angle) {
        this.a = a;
        this.b = b;
        this.angle = Math.toRadians(angle);
    }

    public double getPerimeter() {
        return 2* (a + b);
    }

    public double getArea() {
        return a * b * Math.sin(angle);
    }
}

class Circle extends Figure {
    private double radius;

    Circle(double radius) {
        this.radius = radius;
    }

    @Override
    public double getArea() {
        return radius * radius * Math.PI;
    }

    @Override
    public double getPerimeter() {
        return Math.PI * 2 * radius;
    }
    
    @Override
    public String getName() {
        return "Koło";
    }
}

class Square extends FourSided {
    public Square(double a) {
        super(a, a, 90);
    }

    @Override
    public String getName(){
        return "Kwadrat";
    }
}

class Rhombus extends FourSided {
    public Rhombus(double a, double angle) {
        super(a, a, angle);
    }

    @Override
    public String getName() {
        return "Romb";
    }
}

class Rectangle extends FourSided {
    public Rectangle(double a, double b){
        super(a, b, 90);
    }

    @Override
    public String getName(){
        return "Prostokąt";
    }
}

abstract class RegularPolygon extends Figure {
    protected double side;
    protected int n;

    RegularPolygon(int n, double side) {
        this.n = n;
        this.side = side;
    }

    @Override
    public double getArea() {
        return (getPerimeter() * side) / (4 * Math.tan(Math.PI / n));
    }

    @Override
    public double getPerimeter() {
        return n * side;
    }
}

class Pentagon extends RegularPolygon {
    public Pentagon(double side) {
        super(5, side);
    }

    @Override
    public String getName() {
        return "Pięciokąt";
    }
}

class Hexagon extends RegularPolygon {
    public Hexagon(double side) {
        super(6, side);
    }

    @Override
    public String getName() {
        return "Sześciokąt";
    }
}
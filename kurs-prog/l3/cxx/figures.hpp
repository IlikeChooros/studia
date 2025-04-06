#pragma once

#include <cmath>

constexpr double PI = 3.141592653589793;
constexpr double to_rad(double angle) { return angle * PI / 180.0; }

enum FigureType
{
    None = 0,
    CircleType = 'o',
    FourSidedType = 'c',
    PentagonType = 'p',
    HexagonType = 's',

    RectangleType = 1,
    SquareType = 2,
    RhombusType = 3,
};

inline constexpr const char* figure_type_to_string(FigureType type)
{
    switch (type)
    {
    case CircleType: return "Koło";
    case RectangleType: return "Prostokąt";
    case SquareType: return "Kwadrat";
    case RhombusType: return "Romb";
    case PentagonType: return "Pięciokąt";
    case HexagonType: return "Sześciokąt";
    default: return "Nieznany typ figury";
    }
}


// Base class for all figures
class Figure
{
public:
    virtual double area() const = 0;
    virtual double perimeter() const = 0;
    virtual const char* name() const = 0;
    virtual ~Figure() = default;
};


class Circle : public Figure
{
    double M_radius;
public:
    static constexpr bool check(double radius)
    {
        return radius > 0;
    }

    Circle(double radius) : M_radius(radius) {}
    double area() const override { return 3.14159 * M_radius * M_radius; }
    double perimeter() const override { return 2 * 3.14159 * M_radius; }
    const char* name() const override { return "Koło"; }
};


// Abstract class for all four-sided figures
class FourSided : public Figure
{
protected:
    double M_a, M_b, M_angle;
public:
    static FigureType check(double a, double b, double c, double d, double angle);

    FourSided(double a, double b, double angle)
        : M_a(a), M_b(b), M_angle(to_rad(angle)) {}
    double area() const override { return M_a * M_b * sin(M_angle); }
    double perimeter() const override { return 2 * (M_a + M_b); }
};

class Rectangle : public FourSided
{
public:
    Rectangle(double a, double b) : FourSided(a, b, 90) {}
    double area() const override { return M_a * M_b; }
    const char* name() const override { return "Prostokąt"; }
};

class Square : public Rectangle
{
public:
    Square(double a) : Rectangle(a, a) {}
    double area() const override { return M_a * M_a; }
    const char* name() const override { return "Kwadrat"; }
};

class Rhombus : public FourSided
{
public:
    Rhombus(double a, double angle) : FourSided(a, a, angle) {}
    const char* name() const override { return "Romb"; }
};

// Abstract class for all regular polygons
template <size_t N>
class RegularPolygon : public Figure
{
protected:
    double M_side;
public:
    static_assert(N >= 3, "RegularPolygon must have at least 3 sides");

    RegularPolygon(double side) : M_side(side) {}
    double area() const override { return (perimeter() * M_side) / (4 * tan(PI / N)); }
    double perimeter() const override { return N * M_side; }
};

// Regular pentagon
class Pentagon : public RegularPolygon<5>
{
public:
    Pentagon(double side) : RegularPolygon<5>(side) {}
    const char* name() const override { return "Pięciokąt"; }
};

// Regular hexagon
class Hexagon : public RegularPolygon<6>
{
public:
    Hexagon(double side) : RegularPolygon<6>(side) {}
    const char* name() const override { return "Sześciokąt"; }
};
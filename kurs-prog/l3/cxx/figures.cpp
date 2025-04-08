#include "figures.hpp"

constexpr bool equal(double a, double b)
{
    return std::abs(a - b) < 1e-6;
}


FigureType FourSided::check(double a, double b, double c, double d, double angle)
{
    if (a <= 0 || b <= 0 || c <= 0 || d <= 0)
        return None;
    if (angle < 0 || angle > 180)
        return None;

    // Four sided figure must have opposite sides equal
    if (!equal(a, c) || !equal(b, d))
        return None;
    

    // Check if all sides are equal
    if (equal(a, b) && equal(c, d))
    {
        if (equal(angle, 90))
            return SquareType;
        else
            return RhombusType;
    }

    // Check if the angle is 90 degrees
    if (equal(angle, 90))
        return RectangleType;
    
    // Otherwise, we do not specify the type
    return None;
}
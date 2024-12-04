#include <stdio.h>
#include <math.h>

// Type definitions
typedef double num_t;
typedef num_t (*func_t)(num_t);


// Find zero of the arbitrary function f(x)
// Using the bisection method
double find_zero(func_t function, num_t a, num_t b, num_t eps)
{
    num_t c = 0;

    while (fabs(b - a) > eps)
    {
        c  = (a + b) / 2;
        if (function(a) * function(c) < 0)
            b = c;

        else
            a = c;
    }
    return c;
}

num_t function(num_t x)
{
    return cos(x / 2);
}

int main()
{
    // Find zero of the function f(x) = x^2 - 2
    num_t a = 2;
    num_t b = 4;
    num_t eps = 1e-2;

    printf("Podaj przedzial [a, b] oraz dokladnosc eps: ");
    int scanned = scanf("%lf %lf %lf", &a, &b, &eps);

    if (scanned != 3)
    {
        printf("Blad wczytywania danych\n");
        return 1;
    }
    num_t zero = find_zero(function, a, b, eps);
    printf("Zero: %lf\n", zero);

    return 0;
}
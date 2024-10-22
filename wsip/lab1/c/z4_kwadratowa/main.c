#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

typedef float number_t;

typedef struct {
    number_t x1;
    number_t x2;
} solution_t;

solution_t solve_quadratic(number_t a, number_t b, number_t c)
{
    solution_t solution;
    number_t delta = b * b - 4 * a * c;


    if (delta < 0) // brak rozwiązań
    {
        solution.x1 = NAN;
        solution.x2 = NAN;
    }

    else if (delta == 0) // 2 takie same rozwiązania
    {
        solution.x1 = -b / (2 * a);
        solution.x2 = solution.x1;
    }

    else // 2 różne rozwiązania
    {
        solution.x1 = (-b - sqrtf(delta)) / (2 * a);
        solution.x2 = (-b + sqrtf(delta)) / (2 * a);
    }
    return solution;
}

/**
 * @param solution - wskaźnik na strukturę z rozwiązaniem
 * @return Napis reprezentujący rozwiązanie równania kwadratowego, powinien zostać zwolniony przez free()
 */
char* solution_to_string(solution_t* solution)
{
    char* buffer = (char*)malloc(128 * sizeof(char));
    if (isnan(solution->x1) || isnan(solution->x2))
        strcpy(buffer, "Brak rozwiązań");

    else if (solution->x1 == solution->x2)
        sprintf(buffer, "x1 = x2 = %.4f", solution->x1);
    
    else
        sprintf(buffer, "x1 = %.4f, x2 = %.4f", solution->x1, solution->x2);
    
    return buffer;
}

int main(int argc, char *argv[]) 
{
    number_t a = 0, b = 0, c = 0;

    printf("Podaj współczynniki ax^2 + bx + c (a,b,c): ");
    scanf("%f %f %f", &a, &b, &c);
    solution_t solution = solve_quadratic(a, b, c);
    char* solution_str = solution_to_string(&solution);
    printf("\nWynik: \n\t%s\n", solution_str);
    free(solution_str);
    return 0;
}
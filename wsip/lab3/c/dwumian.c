#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned long long num_t;

// Oblicza symbola newtona = n ! / (k ! * (n - k) !)
// Przy uzyciu tablicy i trojkata pascala
num_t binomial_coefficient(int n, int k)
{
    // wadliwe dane
    if (n < k)
        return 0;

    // inicjalizacja tablicy, z kazdym elementem = 0
    num_t coeffs[k + 1];
    memset(coeffs, 0, sizeof(num_t)*(k + 1));
    coeffs[0] = 1;

    // 'i' poziom w trojkacie, temp 
    num_t i = 1, temp, prev_copy;

    for(num_t i = 1; i <= n; i++)
    {
        // zmienna pomocnicza, by zachowac wartosc z poprzedniego elementu
        prev_copy = 1;
        const num_t min = i < k ? i : k;

        for(num_t j = 1; j <= min; j++)
        {
            // coeffs[j] += coeffs[j-1] dziala tylko dla pierwszego elementu,
            // dlatego potrzebny jest 'temp' i 'prev_copy', by zachowac stan z przed 
            // dodawania
            temp = coeffs[j];
            coeffs[j] = coeffs[j] + prev_copy;
            prev_copy = temp;
        }

        // jesli i <= k, to przypisujemy 1 (koniec trojkata)
        if (i <= k)
            coeffs[i] = 1;
    }

    return coeffs[k];
}

int main(int argc, char* argv[])
{
    int n = 0, k = 0;
    printf("Podaj 2 liczby: ");
    scanf("%d %d", &n, &k);
    printf("Symbol Newtona (%d %d) = %lu\n", n, k, binomial_coefficient(n, k));
    return 0;
}
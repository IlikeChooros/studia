#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>


typedef unsigned long num_t;

/**
 * @brief Podaj n-ta liczbe pierwsza
 * @brief n - numer liczby pierwszej (n >= 1)
 */
num_t n_prime(num_t n)
{
    if (n < 1)
        return 0;

    // Inicjalizacja tablicy
    num_t primes[n];
    memset(primes, 0, sizeof(num_t)*(n));
    primes[0] = 2;

    // Licznik liczb pierwszych w tablicy, liczba do sprawdzenia
    num_t prime_count = 1, num = 3;

    while (prime_count < n)
    {
        const num_t root = (num_t)sqrtf(num);
        int is_prime = 1;

        for (num_t i = 0; i < prime_count; i++)
        {
            if (primes[i] > root)
                break;
            
            if (num % primes[i] == 0)
            {
                is_prime = 0;
                break;
            }
        }

        // Jest pierwsza
        if (is_prime)
            primes[prime_count++] = num;
        
        num += 2;
    }
    
    return primes[n-1];
}


int main(int argc, char* argv[])
{
    int n = 0;
    printf("Podaj liczbe: ");
    scanf("%d", &n);
    printf("%d-ta liczba pierwsza: %lu\n", n, n_prime(n));
}
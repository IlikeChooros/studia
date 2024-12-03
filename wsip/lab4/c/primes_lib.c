#include "primes_lib.h"

// Zlicza ile jest liczb pierwszych w podanym 'sicie'
num_t count_primes(num_t* sieve, num_t size)
{
    num_t sum = 0;
    for(num_t i = 0; i < size; i++)
        sum += sieve[i];
    return sum;
}

// Sprawdza, ile jest liczb pierwszych do 'n'
num_t primes_up_to_n(num_t n)
{
    if (n < 0)
        return -1;

    num_t root = (num_t)sqrt(n) + 1;
    num_t sieve[n + 1];
    
    for (num_t i = 0; i < n + 1; i++)
        sieve[i] = 1;

    sieve[0] =  0;
    sieve[1] =  0;

    for (num_t i = 2; i < root; i++)
        if (sieve[i])
            for (num_t j = i*i; j < n + 1; j += i)
                sieve[j] = 0;

    return count_primes(sieve, n + 1);
}

// Sprawdza, czy liczba `n` jest pierwsza, zwraca 0 - nie, 1 - tak
num_t is_prime(num_t n)
{
    if (n < 0)
        return -1;

    if (n == 2 || n == 3)
        return 1;
    
    if (n % 2 == 0|| n % 3 == 0 || n == 1)
        return 0;

    const num_t root = sqrt(n);

    for (num_t i = 5; i <= root; i += 6)
    {
        if (n % i == 0 || (n % (i + 3) == 0) || n % (i + 2) == 0)
            return 0;
    }

    return 1;
}

/**
 * @brief Podaj n-ta liczbe pierwsza
 * @brief n - numer liczby pierwszej (n >= 1)
 */
num_t nth_prime(num_t n)
{
    if (n < 1)
        return -1;

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
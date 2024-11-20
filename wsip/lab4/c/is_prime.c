#include <stdio.h>
#include <math.h>
#include <string.h>

// Zlicza ile jest liczb pierwszych w podanym 'sicie'
int count_primes(int* sieve, int size)
{
    int sum = 0;
    for(int i = 0; i < size; i++)
        sum += sieve[i];
    return sum;
}

// Sprawdza, ile jest liczb pierwszych do 'n'
int primes_up_to_n(int n)
{
    if (n < 0)
        return -1;

    int root = (int)sqrt(n) + 1;
    int sieve[n + 1];
    
    for (int i = 0; i < n + 1; i++)
        sieve[i] = 1;

    sieve[0] =  0;
    sieve[1] =  0;

    for (int i = 2; i < root; i++)
        if (sieve[i])
            for (int j = i*i; j < n + 1; j += i)
                sieve[j] = 0;

    return count_primes(sieve, n + 1);
}

// Sprawdza, czy liczba `n` jest pierwsza, zwraca 0 - nie, 1 - tak
int is_prime(int n)
{
    if (n < 0)
        return -1;

    if (n == 2 || n == 3)
        return 1;
    
    if (n % 2 == 0|| n % 3 == 0 || n == 1)
        return 0;

    const int root = sqrt(n);

    for (int i = 5; i <= root; i += 6)
    {
        if (n % i == 0 || (n % (i + 3) == 0) || n % (i + 2) == 0)
            return 0;
    }

    return 1;
}

typedef int num_t;

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

#define N_OPTIONS 3

int main(int argc, char* argv[])
{
    // Sprawdz ilosc argumentow 
    if (argc != 3)
    {
        printf("Nie prawidlowa ilosc argumentow\n");
        return -1;
    }

    // Sprawdz poprawnosc argumentu (jezeli pozostanie na -1 to jest blad)
    int argument = -1;
    sscanf(argv[2], "%d", &argument);
    if (argument == -1)
    {
        printf("Nie prawidlowa liczba\n");
        return -1;
    }

    char* function_name = argv[1];
    int result = -1;

    int (*prime_funcs[N_OPTIONS])(int) = {primes_up_to_n, nth_prime, is_prime};
    const char* names[N_OPTIONS]       = {"pn",           "pr",     "ip"};

    for(int i = 0; i < N_OPTIONS; i++)
    {
        if (strcmp(function_name, names[i]) == 0)
        {
            result = prime_funcs[i](argument);
            break;
        }
    }

    if (result == -1)
    {
        printf("Nie znaleziono %s w mozliwych opcjach\n", function_name);
    }
    else
    {
        printf("%d\n", result);
    }

    return 0;
}
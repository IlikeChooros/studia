#include <stdio.h>
#include <math.h>
#include <stdlib.h>
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
    if (n <= 0)
        return -1;

    int root = (int)sqrt(n) + 1;

    // Malloc, bo dla dużych liczb (np. 2 * 10^7) 
    // tworzenie tablicy powoduje stack overflow
    // (segmentation fault)
    int* sieve = (int*)malloc(sizeof(int)*(n + 1));
    
    for (int i = 0; i < n + 1; i++)
        sieve[i] = 1;

    sieve[0] = sieve[1] = 0;

    for (int i = 2; i < root; i++)
        if (sieve[i])
            for (int j = i*i; j < n + 1; j += i)
                sieve[j] = 0;

    int result = count_primes(sieve, n + 1);
    free(sieve);
    return result;
}


int main(int argc, char* argv[])
{
    int n = 2;

    printf("Podaj liczbe: ");
    scanf("%d", &n);
    
    int result = primes_up_to_n(n);
    if (result == -1)
        printf("Podano błędną liczbę \n");
    else 
        printf("Istnieje %d liczb pierwszych od 0 do %d\n", result, n);

    return 0;
}
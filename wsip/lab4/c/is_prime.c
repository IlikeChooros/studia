#include <stdio.h>

#include "primes_lib.h"

void bool_formatter(num_t result)
{
    if (result)
        printf("true\n");
    else
        printf("false\n");
}

void formatter(num_t result)
{
    printf("%ld\n", result);
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

    num_t (*prime_funcs[N_OPTIONS])(num_t) = {primes_up_to_n, nth_prime, is_prime};
    void (*formatters[N_OPTIONS])(num_t)   = {formatter, formatter, bool_formatter};
    const char* names[N_OPTIONS]           = {"pn",           "pr",     "ip"};


    for(int i = 0; i < N_OPTIONS; i++)
    {
        if (strcmp(function_name, names[i]) == 0)
        {
            result = prime_funcs[i](argument);
            formatters[i](result);
            break;
        }
    }

    if (result == -1)
    {
        printf("Niepoprawna opcja: %s\n", function_name);
    }

    return 0;
}
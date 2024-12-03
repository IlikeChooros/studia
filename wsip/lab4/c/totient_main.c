#include <stdio.h>

#include "totient_lib.h"

int main(int argc, char* argv[])
{
    if (argc <= 1)
    {
        printf("Nieprawidlowa ilosc argumentow\n");
        return -1;
    }


    for (int i = 1; i < argc; i++)
    {
        num_t n     = 0;
        int scanned = sscanf(argv[i], "%u", &n);

        if (scanned != 1)
        {
            printf("Nieprawidlowa liczba: %s\n", argv[i]);
            continue;
        }

        printf("totient(%u) = %u\n", n, totient(n));
    }
    return 0;
}
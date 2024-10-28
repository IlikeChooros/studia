#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// Sprawdza, czy liczba `n` jest pierwsza, zwraca 0 - nie, 1 - tak, -1 - błąd
int is_prime(int n)
{
    if (n <= 0)
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


int main(int argc, char* argv[])
{
    int n = 2;

    printf("Podaj liczbe: ");
    scanf("%d", &n);
    
    int result = is_prime(n);
    if (result == -1)
        printf("Podano błędną liczbę \n");
    else if (result == 1)
        printf("Liczba %d jest pierwsza \n", n);
    else 
        printf("%d nie jest pierwsza \n", n);

    return 0;
}
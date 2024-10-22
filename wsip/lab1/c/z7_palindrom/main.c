#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/**
 * @param n - liczba naturalna
 * @return true jeśli liczba `n` jest palindromem w systemie `sys`, w przeciwnym wypadku false
 */
int palindrom(int n, int sys)
{
    if (n < 0)
        return 0;

    if (sys < 2)
        return 0;

    int reversed = 0;
    int original = n;
    while (n > 0)
    {
        reversed = reversed * sys + n % sys;
        n /= sys;
    }

    return original == reversed;
}


int main(int argc, char *argv[]) 
{
    int n, sys;
    printf("Podaj liczbę naturalną i system: ");
    scanf("%d", &n);
    scanf("%d", &sys);

    const int result = palindrom(n, sys);

    if (result != 0)
        printf("Liczba %d jest palindromem w systemie %d\n", n, sys);
    else
        printf("Liczba %d nie jest palindromem\n", result);

    return 0;
}
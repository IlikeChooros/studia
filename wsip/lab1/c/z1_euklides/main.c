#include <stdio.h>

/**
 * @param a - liczba naturalna
 * @param b - liczba naturalna
 * @return Największy wspólny dzielnik liczb a i b, lub -1 jeśli dane nieprawidłowe
 */
int euklides_nwd(int a, int b)
{
    if (a <= 0 || b <= 0)
        return -1; // Wartość ujemna oznacza błąd

    while (b != 0)
    {
        int c = a % b;
        a = b;
        b = c;
    }

    return a;
}


int main(int argc, char *argv[]) 
{
    const int a = 18, b = 29;
    printf("NWD(%d, %d) = %d\n", a, b, euklides_nwd(a, b));
    return 0;
}
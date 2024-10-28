#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// Oblicza symbola newtona = n ! / (k ! * (n - k) !)
int binomial_coefficient(int n, int k)
{
    if (n < 0 || k < 0)
        return -1;
    if (k > n)
        return -1;

    int result = 1;
    for (int i = 1; i <= k; i++)
    {
        result *= n - i + 1;
        result /= i;
    }

    return result;
}

int main(int argc, char* argv[])
{
    int n = 0, k = 0;
    printf("Podaj 2 liczby: ");
    scanf("%d %d", &n, &k);
    printf("Symbol Newtona (%d %d) = %d\n", n, k, binomial_coefficient(n, k));
    return 0;
}
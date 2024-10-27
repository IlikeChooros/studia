#include <stdio.h>

/**
 * @param a liczba naturalna
 * @param b liczba naturalna
 * @return NWD(a, b)
 */
int euklides(int a, int b)
{
    if (a < 0 || b < 0)
        return 0;
    
    while(b != 0)
    {
        if (a < b)
        {
            int c = a;
            a = b;
            b = c;
        }
        
        a = a - b;
    }

    return a;
}



int main(int argc, char * argv[])
{
    int a = 0, b = 0;

    printf("Podaj 2 liczby: ");
    scanf("%d %d", &a, &b);

    printf("NWD(%d, %d) = %d\n", a, b, euklides(a, b));

    return 0;
}
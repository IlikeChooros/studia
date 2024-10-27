import math


def prime_distribution(n: int) -> list[int]:
    """
    Zwraca rozkład liczby 'n' na czynniki pierwsze.
    """

    dist: list[int] = []

    if n <= 0:
        return dist
    
    div: int = 2

    # Check for even numbers
    while n > 1 and n % div == 0:
        dist.append(div)
        n //= div
    
    # Set the root of n as the upper limit for the divisors
    root: int = int(math.sqrt(n))
    div = 3

    # Check for odd numbers
    while n > 1 and div <= root:
        if n % div == 0:
            dist.append(div)
            n //= div
            root = int(math.sqrt(n))
        else:
            div += 2
    
    # If n is a prime number greater than 2
    if n > 1:
        dist.append(n)

    return dist


def print_format(n: int, occurance: int, first: bool) -> None:
    """
    Wypisuje liczbę w formacie potęgowym.
    """
    if not first:
        print('*', end='')
    if occurance > 1:
        print(f'{n}^{occurance}', end='')
    else:
        print(n, end='')


def print_distribution(dist: list[int]) -> None:
    """
    Wypisuje rozkład liczby na czynniki pierwsze.
    W formacie potęgowym, np. 2^3 * 3^2 * 5
    """

    if not dist:
        print('Brak czynników pierwszych')
        return

    first: bool = True
    occurence: int = 0
    prev: int = dist[0]

    for n in dist:
        if n == prev:
            occurence += 1
        else:
            print_format(prev, occurence, first)

            prev = n
            occurence = 0
            first = False
    
    print_format(prev, occurence, first)
    print()


def main():
    n: int = int(input("Podaj liczbę: "))

    dist: list[int] = prime_distribution(n)

    print_distribution(dist)


if __name__ == '__main__':
    main()
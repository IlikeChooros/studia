import math

def count_primes(primes: list[bool]) -> int:
    """
    Zlicza ilość liczb pierwszych w podanej liście.
    """
    return sum(primes)


def primes_up_to_n(n: int) -> bool:
    """
    Sprawdza, czy podadana liczba jest pierwsza, z optymalizacją wielkrotności 6.
    """

    if n < 0:
        raise ValueError('Liczba musi być większa od 0')

    root: int = int(math.sqrt(n)) + 1
    sieve: list = [1 for _ in range(n + 1)]
    sieve[0] = sieve[1] = 0

    for i in range(2, root):
        if sieve[i]:
            for j in range(i * i, n + 1, i):
                sieve[j] = 0
    
    return count_primes(sieve)


def main():
    n: int = int(input("Podaj liczbę: "))

    try:
        print(f'Liczba liczb pierwszych do {n}: {primes_up_to_n(n)}')
    except ValueError as e:
        print(e)


if __name__ == '__main__':
    main()

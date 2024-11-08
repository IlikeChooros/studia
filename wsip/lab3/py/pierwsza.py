import math


def is_prime(n: int) -> bool:
    """
    Sprawdza, czy podadana liczba jest pierwsza, z optymalizacją wielkrotności 6.
    """

    if n <= 0:
        raise ValueError('Liczba musi być większa od 0')

    if n == 2 or n == 3:
        return True

    if n % 2 == 0 or n % 3 == 0 or n == 1:
        return False

    root: int = int(math.sqrt(n))

    for i in range(5, root + 1, 6):
        if n % i == 0 or n % (i + 3) == 0 or n % (i + 2) == 0:
            return False

    return True


def main():
    n: int = int(input("Podaj liczbę: "))

    try:
        if is_prime(n):
            print(f'Liczba {n} jest pierwsza')
        else:
            print(f'{n} nie jest pierwsza')
    except ValueError as e:
        print(e)


if __name__ == '__main__':
    main()

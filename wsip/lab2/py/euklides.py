
def euklides(a: int, b: int) -> int:
    """
    Oblicza NWD, za pomocą algorytmu Euklidesa
    """
    if a < 0 or b < 0:
        return 0

    while b > 0:
        if a < b:
            a, b = b, a
        a = a - b
    
    return a


def main():
    a: int = int(input('Podaj pierwszą liczbę: '))
    b: int = int(input('Podaj drugą liczbę: '))

    print(f'NWD({a}, {b}) = {euklides(a, b)}')


if __name__ == '__main__':
    main()
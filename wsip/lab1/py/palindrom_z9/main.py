

def palindrom(a: int, sys: int = 10) -> bool:
    """
    Sprawdza, czy liczba a jest palindromem w systemie o podstawie sys
    """
    if a < 0:
        raise ValueError("Argument must be non-negative")
    if a == 0:
        return True
    if a % sys == 0:
        return False

    original = a
    a_reversed = 0
    while original > 0:
        a_reversed = a_reversed * sys + original % sys
        original //= sys
    return a == a_reversed


def main():
    a: int = int(input('Podaj liczbę: '))
    sys: int = int(input('Podaj system: '))
    
    if palindrom(a, sys):
        print(f'Liczba {a} jest palindromem w systemie {sys}')
    else:
        print(f'Liczba {a} nie jest palindromem')

if __name__ == "__main__":
    main()
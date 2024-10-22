

def euklides_nwd(a: int, b: int) -> int:
    """
    Bierze dwie liczby całkowite a, b i zwraca ich największy wspólny dzielnik
    """
    if a < 0 or b < 0:
        raise ValueError("Arguments must be non-negative")

    while b != 0:
        a = a % b # a = a mod b, więc a jest resztą z dzielenia a przez b
        a, b = b, a # zamiana wartości a i b
    return a


def main():
    # Działa dla liczb nieujemnych
    a = 6
    b = 13
    print(f"NWD({a}, {b}) = {euklides_nwd(a, b)}")


if __name__ == "__main__":
    main()
import numpy as np

def nth_prime(n: int) -> np.int64:
    """
    Oblicza n-tą liczbę pierwszą
    """

    if n < 1:
        raise ValueError(f'Niepoprawne dane, {n} < 1')

    primes      = np.zeros(n, dtype=np.int64)
    primes[0]   = 2
    prime_count = 1
    candidate   = 3

    while prime_count < n:
        is_prime = True

        for i in range(0, prime_count):
            # Sprawdzamy czy dzielnik jest wiekszy od pierwiastka z liczby
            # Wtedy nie ma sensu sprawdzanie dalszych dzielników
            if primes[i] ** 2 > candidate:
                break
            
            # Jeżeli liczba jest podzielna przez którąś 
            # z wcześniej znalezionych liczb pierwszych
            # to nie jest liczbą pierwszą
            if candidate % primes[i] == 0:
                is_prime = False
                break

        if is_prime:
            primes[prime_count] = candidate
            prime_count += 1

        candidate += 2

    return primes[n - 1]


def main():
    n: int = int(input('Podaj n: '))
    
    try:
        print(f'{n}-ta liczba pierwsza: {nth_prime(n)}')
    except ValueError as e:
        print(e)


if __name__ == '__main__':
    main()
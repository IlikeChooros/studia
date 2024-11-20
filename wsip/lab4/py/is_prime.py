import sys
import math
import numpy as np

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

def nth_prime(n: int) -> np.int64:
    """
    Oblicza n-tą liczbę pierwszą
    """

    if n < 1:
        raise ValueError(f'Niepoprawne dane, {n} < 1')

    primes = np.zeros(n, dtype=np.int64)
    primes[0] = 2
    prime_count = 1
    candidate = 3

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
    if len(sys.argv) == 3:
        return -1
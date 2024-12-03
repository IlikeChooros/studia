import math

def prime_factors(n: int) -> dict[int, int]:
    """
    Zwraca rozkład liczby 'n' na czynniki pierwsze.
    """

    factors: dict[int, int] = {}

    if n <= 0:
        return factors
    
    div: int = 2

    # Check for even numbers
    while n > 1 and n % div == 0:
        factors[div] = factors.get(div, 0) + 1
        n //= div
    
    # Set the root of n as the upper limit for the divisors
    root: int = int(math.sqrt(n))
    div: int  = 3

    # Check for odd numbers
    while n > 1 and div <= root:
        if n % div == 0:
            factors[div] = factors.get(div, 0) + 1
            n //= div
            root = int(math.sqrt(n))
        else:
            div += 2
    
    # If n is a prime number greater than 2
    if n > 1:
        factors[n] = factors.get(n, 0) + 1

    return factors

def totient(n: int) -> int:
    """
    Oblicza funkcję Eulera dla liczby 'n'.
    """

    if n < 1:
        raise ValueError('Liczba musi być większa od 0')

    factors: dict[int, int] = prime_factors(n)
    result : int            = 1

    for div, power in factors.items():
        result *= div**(power - 1) * (div - 1)

    return int(result)

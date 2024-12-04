import numpy as np


def binomial_coefficient(n: int, k: int) -> np.int64:
    """
    Oblicza wartość dwumianu Newtona (n k) = n! / (k! * (n - k)!)
    Przy użyciu trójkąta Pascala, zwraca wartość dwumianu dla pary (n, k).
    """

    if n < k:
        raise ValueError(f'Niepoprawne dane, {n} < {k}')
    
    # Inicjalizacja tablicy dwuwymiarowej z 0, n + 1 wierszy, k + 1 kolumn,
    # Ponieważ chcemy obliczyć wartość dwumianu dla pary (n, k)
    coeffs = np.zeros((k + 1), dtype=np.int64)
    coeffs[0] = 1

    for i in range(1, n + 1):        
        # min(i, k) + 1, ucinamy wartości, które nie mają sensu
        # zatem dla danego poziomu n, obliczamy wartości dla k od 1 do min(i, k)
        # a nie cały zakres tego poziomu ( = i + 1)
        prev_copy = 1
        for j in range(1, min(i, k) + 1):
            temp = coeffs[j]
            coeffs[j] += prev_copy
            prev_copy = temp
        
        if i <= k:
            coeffs[i] = 1

    return coeffs[k]



def main():
    n: int = int(input('Podaj n: '))
    k: int = int(input('Podaj k: '))

    try:
        result = binomial_coefficient(n, k)
        print(f'({n} {k}): {result}')
    except ValueError as e:
        print(e)


if __name__ == '__main__':
    main()
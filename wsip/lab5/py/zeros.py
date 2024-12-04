import typing as tp
import math

def find_zero(function: tp.Callable[[float], float], x0: float, x1: float, eps: float = 1e-7) -> tp.Optional[float]:
    """
    
    Znajduje miejsce zerowe funkcji f(x) na przedziale [x0, x1] z zadaną dokładnością eps.
    Metoda bisekcji.
    
    """

    # Sprawdzenie czy funkcja zmienia znak na zadanym przedziale
    if function(x0) * function(x1) > 0:
        return None
    
    # Algorytm bisekcji
    while abs(x0 - x1) > eps:
        x2: float = (x0 + x1) / 2
        if function(x0) * function(x2) < 0:
            x1 = x2
        else:
            x0 = x2
    
    return x1


def function(x: float) -> float:
    return math.cos(x/2)


def main():
    x0: float = 2
    x1: float = 4
    eps: float = 1e-7

    x0, x1 = int(input("Podaj początek przedziału: ")), int(input("Podaj koniec przedziału: "))
    x0, x1 = min(x0, x1), max(x0, x1)

    zero: tp.Optional[float] = find_zero(function, x0, x1, eps)
    if zero is not None:
        print(f"Znalezione miejsce zerowe: {zero}")
    else:
        print("Nie udało się znaleźć miejsca zerowego na zadanym przedziale.")

if __name__ == "__main__":
    main()
import math

class Solution:
    """
    Przechowuje rozwiązanie równania kwadratowego
    """

    x1: float
    x2: float
    valid: bool

    def __init__(self, x1: float, x2: float, valid: bool = True):
        self.x1 = x1
        self.x2 = x2
        self.valid = valid
    
    def __str__(self) -> str:
        if self.valid:
            return f'x1 = {self.x1:.4f}, x2 = {self.x2:.4f}'
        else:
            return 'Brak rozwiązań'

def read_input() -> tuple[float, float, float]:
    """
    Wczytuje współczynniki równania kwadratowego (a, b, c)
    """
    a = float(input('Podaj a: '))
    b = float(input('Podaj b: '))
    c = float(input('Podaj c: '))
    return a, b, c


def solve_quadratic(a: float, b: float, c: float) -> Solution:
    """
    Rozwiązuje równanie kwadratowe ax^2 + bx + c = 0

    @param a: współczynnik przy x^2
    @param b: współczynnik przy x
    @param c: wyraz wolny
    @return: obiekt klasy Solution
    """
    delta: float = b**2 - 4*a*c
    if delta < 0:
        return Solution(0, 0, False)
    x1: float = (-b - math.sqrt(delta)) / (2*a)
    x2: float = (-b + math.sqrt(delta)) / (2*a)
    return Solution(x1, x2)


def main():
    print(f'Wynik: \n\t{solve_quadratic(*read_input())}')


if __name__ == "__main__":
    main()
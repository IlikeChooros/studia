import sys
import typing


def is_valid_solution(perm: list[int]) -> bool:
    """
    Funkcja sprawdzająca, czy ustawienie hetmanów na szachownicy jest poprawne.
    :param perm: lista permutacji
    """
    for i in range(len(perm)):
        for j in range(i + 1, len(perm)):
            # Sprawdzamy, czy hetmany nie stoją na tych samych wierszach lub przekątnych
            if abs(perm[i] - perm[j]) == abs(i - j):
                return False

    return True


def count_perm_queen(n: int, k: int, perm: list[int], used: list[bool]) -> int:
    """
    Funkcja rekurencyjna zliczająca rozwiązania problemu n-hetmanów.
    :param n: liczba hetmanów
    :param k: numer hetmana
    :param perm: lista permutacji pozycji hetmanów
    :param used: lista użytych pozycji
    :return: liczba rozwiązań problemu n-hetmanów
    """
    n_solutions: int = 0
    if k == n:
        if is_valid_solution(perm):
            print(perm)
            return 1
    else:
        for i in range(n):
            if not used[i]:
                used[i] = True
                perm[k] = i
                n_solutions += count_perm_queen(n, k + 1, perm, used)
                used[i] = False

    return n_solutions


def perm_queen(n: int) -> int:
    """
    Funkcja zwracająca liczbę rozwiązań problemu n-hetmanów,
    z wykorzystaniem permutacji.
    :param n: liczba hetmanów
    :return: liczba rozwiazań problemu
    """
    perm: list[int] = [0 for _ in range(n)]
    used: list[bool] = [False for _ in range(n)]

    return count_perm_queen(n, 0, perm, used)


def put_queen(sq: int, position: list[int], att_row: list[bool], att_d1: list[bool], att_d2: list[bool], size: int) -> bool:
    """
    Funckja zliczjąca rozwiązania porblemu n-hetmanów z nawrotami.
    :param sq: numer hetmana
    :param position: lista pozycji hetmanów
    :param att_row: lista zajętych wierszy
    :param att_d1: lista zajętych przekątnych
    :param att_d2: lista zajętych przekątnych
    :param size: liczba hetmanów (i rozmiar szachownicy)
    """

    n_solutions: int = 0

    for i in range(size):
        if not att_row[i] and not att_d1[sq + i] and not att_d2[sq - i + size]:
            # Ustawiamy hetmana
            position[sq] = i
            att_row[i] = att_d1[sq + i] = att_d2[sq - i + size] = True

            if sq == size - 1:
                # Znaleźliśmy rozwiązanie
                print(position)
                n_solutions += 1
            else:
                # Rekurencyjnie szukamy kolejnych rozwiązań
                n_solutions += put_queen(sq + 1, position,
                                         att_row, att_d1, att_d2, size)
            # Usuwamy hetmana
            att_row[i] = att_d1[sq + i] = att_d2[sq - i + size] = False

    return n_solutions


def n_queen(size: int) -> int:
    """
    Funkcja zwracająca liczbę rozwiązań problemu n-hetmanów.
    :param size: liczba hetmanów (i rozmiar szachownicy)
    :return: liczba rozwiazań problemu
    """
    position: list[int] = [0 for _ in range(size)]
    att_row: list[bool] = [False for _ in range(size)]
    att_d1:  list[bool] = [False for _ in range(2 * size)]
    att_d2:  list[bool] = [False for _ in range(2 * size)]

    return put_queen(0, position, att_row, att_d1, att_d2, size)


def main() -> None:
    """
    Funkcja główna
    """
    if len(sys.argv) != 3:
        print("Usage: python3 %s [perm | back] <n>" % sys.argv[0])
        sys.exit(1)

    method: str = sys.argv[1]
    n:      int = int(sys.argv[2])

    assert method in ["perm", "back"], "Invalid method, use 'perm' or 'back'"

    functions: dict[str, typing.Callable[[int], int]] = {
        "perm": perm_queen,
        "back": n_queen
    }

    print("Number of solutions:", functions[method](n))


if __name__ == "__main__":
    main()

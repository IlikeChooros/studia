import sys
from numeric.primes import is_prime, primes_up_to_n, nth_prime

def main():
    if len(sys.argv) != 3:
        print('Niepoprawna liczba argumentów, użycie: python main.py ip|pr|pn <liczba>')
        return
    
    comm: str = sys.argv[1]
    
    try:
        n: int = int(sys.argv[2])
    except ValueError:
        print('Niepoprawny argument, podaj liczbę całkowitą')
    
    try:
        if comm == 'ip':
            result: bool = is_prime(n)
            print('true' if result else 'false')
        elif comm == 'pr':
            print(nth_prime(n))
        elif comm == 'pn':
            print(primes_up_to_n(n))
        else:
            print('Niepoprawna komenda {}' % comm)
    except ValueError as e:
        print(e)

    return

if __name__ == '__main__':
    main()
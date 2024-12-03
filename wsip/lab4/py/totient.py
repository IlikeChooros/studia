import sys
from numeric.totient import totient


def main():
    if len(sys.argv) <= 1:
        print('Podaj liczbe/y do obliczenia funkcji Eulera')
        return
    
    for arg in sys.argv[1:]:
        try:
            n: int = int(arg)
            print(f'totient({n}) = {totient(n)}')
        except ValueError:
            print(f'Niepoprawny argument: {arg}')

if __name__ == '__main__':
    main()
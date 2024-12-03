
#### Opcja 1
```bash
gcc primes_lib.c -o is_prime
```

#### Opcja 2
```bash
gcc -c primes_lib.c -o primes_lib.o
gcc -c is_prime.c -o is_prime.o
gcc primes_lib.o is_prime.o -o is_prime
```

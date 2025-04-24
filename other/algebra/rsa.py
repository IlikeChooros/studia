
"""
Tekst został zakodowany Twoim kluczem publicznym. Wiadomo, że kodowano oddzielnie wszystkie
litery (najpierw przekształcono je na kody UTF-8, potem otrzymane liczby podniesiono do pewnej po-
tęgi modulo 101080891 i otrzymane liczby zapisano w układzie szesnastkowym i połączono je w jeden
łańcuch, oddzielając poszczególne podłańcuchy dwukropkiem. Twoim kluczem prywatnym jest para
(2062465, 101080891).

1. Odkoduj ten tekst.
2. Znajdź faktoryzację liczby 101080891.
3. Jaki jest Twój klucz publiczny?
"""

"""
Algorytm RSA

(Szyfrowanie)
1. p, q - liczby pierwsze
2. n = p * q
3. phi(n) = (p - 1) * (q - 1)
4. wybierz a z grupy Z_phi(n)*, oblicz b takie, że a * b = 1 (mod phi(n))
5. Klucz prywatny: (a, n), klucz publiczny: (b, n)

Dla wiadomości:
0xXXXXX...XX:0xXXX... itd. 

Podziel wiadomość na podłańcuchy
następnie każdy podłańcuch zaszyfruj:

S_i = M_i ^ b (mod n), gdzie M_i to (w tym przypadku znak z łańcucha UTF-8 w postaci binarnej)

S = S_1:S_2:...:S_n

(Deszyfrowanie)
M_i = S_i ^ a (mod n), gdzie S_i to podłańcuch zaszyfrowany
M = M_1:M_2:...:M_n

"""

PUBLIC_KEY = (2062465, 101080891) # (b, n)

def decode_rsa(encoded_message: str, public_key: tuple) -> str:
    """
    Decode the given encoded message using the RSA algorithm with the provided public key.

    :param encoded_message: The encoded message in hexadecimal format.
    :param public_key: A tuple containing the public key (b, n).
    :return: The decoded message as a string.
    """
    b, n = public_key
    hex_values = encoded_message.split(':')
    decoded_chars = []

    # Iterate over each hex value
    # decoded_chars = [chr(pow(int(hex_value, 16), b, n)) for hex_value in hex_values]

    for hex_value in hex_values:
        # Convert hex to int
        S_i = int(hex_value, 16)
        # Decrypt using the private key
        M_i = pow(S_i, b, n)
        # Convert int to character
        decoded_chars.append(chr(M_i))

    return ''.join(decoded_chars)


def factorization(n: int) -> tuple:
    """
    Factorize the given number n into its prime factors.

    :param n: The number to be factorized.
    :return: A tuple containing the prime factors of n.
    """
    factors = []
    d = 2
    while d * d <= n:
        if n % d == 0:
            factors.append(d)
            n //= d
        else:
            d += 1
    if n > 1:
        factors.append(n)
    return tuple(factors)

def inverse_mod(a: int, m: int) -> int:
    """
    Compute the modular inverse of a modulo m using the Extended Euclidean Algorithm.
    a * x ≡ 1 (mod m)

    :param a: The number to find the modular inverse of.
    :param m: The modulus.
    :return: The modular inverse of a modulo m.
    """
    
    m0, x0, x1 = m, 0, 1
    if m == 1:
        return 0
    
    # Apply extended Euclidean algorithm
    while a > 1:
        # q is quotient
        q = a // m
        m, a = a % m, m
        x0, x1 = x1 - q * x0, x0
    if x1 < 0:
        x1 += m0
    return x1


def main():
    import os, sys
    ABS_PATH = os.path.abspath(sys.argv[0])
    MSG_FILE = os.path.join(os.path.dirname(ABS_PATH), 'msg.txt')

    if not os.path.exists(MSG_FILE):
        print(f"File {MSG_FILE} does not exist.")
        return
    
    # Read the encoded message from the file
    file = open(MSG_FILE, 'r')
    encoded_message = file.read()
    file.close()
    print("Encoded: \n", encoded_message, sep=None)

    # Decode the message using the public key
    decoded_message = decode_rsa(encoded_message, PUBLIC_KEY)
    print("Decoded: \n", decoded_message, sep=None)


    # Factorize the number n
    n = PUBLIC_KEY[1]
    factors = factorization(n)
    print("Factors of n:", factors)

    # Calculate the private key
    p, q = factors
    phi_n = (p - 1) * (q - 1)
    b = PUBLIC_KEY[0]
    a = inverse_mod(b, phi_n)
    print("Private key (a, n):", (a, n))

if __name__ == "__main__":
    main()
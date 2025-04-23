
"""
Tekst został zakodowany Twoim kluczem publicznym. Wiadomo, że kodowano oddzielnie wszystkie
litery (najpierw przekształcono je na kody UTF-8, potem otrzymane liczby podniesiono do pewnej po-
tęgi modulo 101080891 i otrzymane liczby zapisano w układzie szesnastkowym i połączono je w jeden
łańcuch, oddzielając poszczególne podłańcuchy dwukropkiem. Twoim kluczem prywatnym jest para
(2062465, 101080891).
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

if __name__ == "__main__":
    main()
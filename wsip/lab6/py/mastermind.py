
MAX_CODES = 6 ** 4

def gen_numbers() -> list[int]:
    numbers: list[int] = [1] * MAX_CODES

    for i in range(0, MAX_CODES):
        code: int = 1111

        for k in range(3, -1, -1):
            # This is basically conversion of the 'i' to
            # base 6 number
            code += ((i // int(6 ** k)) % 6) * (10 ** k)
        numbers[i] = code
    return numbers

def solve(numbers: list[int], n_in_place: int, n_out_of_place: int) -> None:
    """
    Solve one step of the game, with the given feedback
    """
    guess: int = numbers[0]
    filtered: list[int] = []

    for i in range(len(numbers)):
        code: int = numbers[i]
        in_place: int = 0
        out_of_place: int = 0

        for k in range(3, -1, -1):
            guess_digit: int = (guess // int(10 ** k)) % 10
            code_digit:  int = (code  // int(10 ** k)) % 10

            # If the code's digit is the same as the guess's
            # then increase number of `in place` digits
            if code_digit == guess_digit:
                in_place += 1
            
            # Else, see if there are any `out of place`
            # digits
            else:
                for j in range(3, -1, -1):
                    guess_digit_j: int = (guess // 10 ** j) % 10
                    if guess_digit_j == code_digit:
                        out_of_place += 1
        
        if in_place == n_in_place and out_of_place == n_out_of_place:
            filtered.append(code)

    return filtered


def print_guess(numbers: list[int], n_guess: int = 1):
    print("Guess: ", numbers)
    num: int = numbers[0]
    print("%d: %d %d %d %d ?" % (
        n_guess, (num // 10 ** 3) % 10,
        (num // 10 ** 2) % 10, (num // 10) % 10, 
        num % 10
    ))

def main():
    numbers: list[int] = gen_numbers()
    in_place: int = 0
    out_of_place: int = 0
    n_guess: int = 1
    
    while not (in_place == 4 or len(numbers) == 0):
        print_guess(numbers, n_guess)

        in_place = int(input("Na swoim miejscu: "))
        out_of_place = int(input("Nie na swoim miejscu: "))

        numbers = solve(numbers, in_place, out_of_place)
        n_guess += 1

    if in_place == 4:
        print("Wygrałem.")
    else:
        print("Oszukujesz.")


if __name__ == '__main__':
    main()
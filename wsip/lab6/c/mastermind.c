#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#define N_SLOTS 4
#define N_DIGITS 6
#define MIN_GUESS 1111

static int *all_codes  = NULL;
static int n_all_codes = 0;

/**
 * @brief Generate all possible codes
 */
void generate_all()
{
    n_all_codes = pow(N_DIGITS, N_SLOTS);
    all_codes = (int *)malloc(n_all_codes * sizeof(int));

    for (int i = 0; i < n_all_codes; i++)
    {
        all_codes[i] = MIN_GUESS;
        for (int k = N_SLOTS - 1; k >= 0; k--)
        {
            // Convert i of base 10, to base 6
            all_codes[i] += ((i / (int)pow(N_DIGITS, k)) % N_DIGITS) * pow(10, k);
        }
        printf("%d: %d\n", i, all_codes[i]);
    }
}


/**
 * @brief Solve one step of the game, with the given feedback
 */
void solve(int guess, int feedback_in_place, int feedback_out_of_place)
{
    // new number of codes
    int n = 0;

    for (int i = 0; i < n_all_codes; i++)
    {
        int code = all_codes[i];
        int in_place = 0;
        int out_of_place = 0;

        // Check every digit of the guess
        for (int j = N_SLOTS - 1; j >= 0; j--)
        {
            int digit_guess = (guess / (int)pow(10, j)) % 10;
            int digit_code  = (code  / (int)pow(10, j)) % 10;

            // Check if the digit is in place (same as the previous guess)
            if (digit_guess == digit_code)
            {
                in_place++;
            }

            // Else, check if it is out of place (same as another digit in the guess)
            else
            {
                for (int k = N_SLOTS - 1; k >= 0; k--)
                {
                    int digit_guess_k = (guess / (int)pow(10, k)) % 10;
                    if (digit_guess_k == digit_code)
                    {
                        out_of_place++;
                        break;
                    }
                }
            }
        }

        // See if that code meets the requirements
        if (in_place == feedback_in_place && out_of_place == feedback_out_of_place)
        {
            all_codes[n++] = code;
        }
    }

    // update the number of codes
    n_all_codes = n;
}


// Print the guess
void print_code(int code, int n_guess)
{
    printf("%d: ", n_guess);
    for (int i = 0; i < N_SLOTS; i++)
    {
        printf("%d ", (code / (int)pow(10, N_SLOTS - i - 1)) % 10);
    }
    printf("?\n");
}


int main(int argc, char *argv[])
{
    // Initialize the game
    generate_all();

    int guess                 = all_codes[0],
        feedback_in_place     = 0,
        feedback_out_of_place = 0,
        n_guess               = 1;
    
    while (!(feedback_in_place == N_SLOTS || n_all_codes == 0))
    {
        // Print the guess
        print_code(guess, n_guess++);

        printf("Na swoim miejscu: ");
        scanf("%d", &feedback_in_place);
        printf("Nie na swoim miejscu: ");
        scanf("%d", &feedback_out_of_place);

        // Solve the puzzle
        solve(guess, feedback_in_place, feedback_out_of_place);

        // Make the next guess
        guess = all_codes[0];
    }

    // Print the solution or the error message
    if (n_all_codes == 1)
    {
        printf("Wygrałem.\n");
    }
    else
    {
        printf("Oszukujesz.\n");
    }

    return 0;
}
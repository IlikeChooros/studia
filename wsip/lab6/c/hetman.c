#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

/**
 * @brief Check if the permutation is a valid solution for the n-queen problem
 */
bool is_valid(int *perm, int n)
{
    for (int i = 0; i < n; i++)
    {
        for (int j = i + 1; j < n; j++)
        {
            // Check if the queens are on the same row or diagonal
            if (abs(perm[i] - perm[j]) == j - i)
                return false;
        }
    }
    return true;
}

/**
 * @brief Generate all permutations of the numbers 1 to n
 * O(n!) time complexity
 *
 * @param n The number of elements
 * @param k The current position
 * @param a The array of elements
 * @param used The array of used digits
 * @return int The number of solutions
 */
int permutation(int n, int k, int *a, int *used)
{
    int n_solutions = 0;
    int i;

    if (k == n)
    {
        if (is_valid(a, n))
        {
            for (i = 0; i < n; i++)
                printf("%d ", a[i]);
            printf("\n");
            return 1;
        }
    }
    else
    {
        for (i = 0; i < n; i++)
        {
            if (!used[i])
            {
                a[k] = i + 1;
                used[i] = 1;
                n_solutions += permutation(n, k + 1, a, used);
                used[i] = 0;
            }
        }
    }

    return n_solutions;
}

/**
 * @brief Generate all solutions for the n-queen problem using permutations
 *
 * @param n The number of elements
 * @return int The number of solutions
 */
void perm_queen(int n)
{
    int *a = (int *)malloc(n * sizeof(int));
    int *used = (int *)malloc(n * sizeof(int));

    for (int i = 0; i < n; i++)
        used[i] = 0;

    int solutions = permutation(n, 0, a, used);
    printf("Number of solutions: %d\n", solutions);
    free(a);
    free(used);
}

/**
 * @brief Put a queen on the board recursively, until all queens are placed
 */
int put_queen(int sq, int *position, bool *att_row, bool *att_d1, bool *att_d2, int n)
{
    int n_solutions = 0;
    for (int i = 0; i < n; i++)
    {
        // See if the queen can be placed on the current square
        if (!att_row[i] && !att_d1[sq + i] && !att_d2[n + sq - i])
        {
            // Place the queen
            position[sq] = i;
            att_row[i] = att_d1[sq + i] = att_d2[n + sq - i] = true;

            // If all queens are placed, print the solution
            if (sq == n - 1)
            {
                for (int j = 0; j < n; j++)
                    printf("%d ", position[j]);
                printf("\n");
                n_solutions++;
            }
            else
            {
                // Recursively place the next queen
                n_solutions += put_queen(sq + 1, position, att_row, att_d1, att_d2, n);
            }

            // Remove the queen
            att_row[i] = att_d1[sq + i] = att_d2[n + sq - i] = false;
        }
    }

    return n_solutions;
}

/**
 * @brief Generate all solutions for n-queen problem using backtracking, O(n^n) time complexity
 */
void n_queen(int n)
{
    int *position = (int *)malloc(n * sizeof(int));
    bool *att_row = (bool *)malloc(n * sizeof(bool));
    bool *att_d1 = (bool *)malloc(2 * n * sizeof(bool));
    bool *att_d2 = (bool *)malloc(2 * n * sizeof(bool));

    for (int i = 0; i < n; i++)
    {
        position[i] = 0;
        att_row[i] = false;
    }

    for (int i = 0; i < 2 * n; i++)
        att_d1[i] = att_d2[i] = false;

    int n_solutions = put_queen(0, position, att_row, att_d1, att_d2, n);
    printf("Number of solutions: %d\n", n_solutions);
}

int main(int argc, char *argv[])
{
    void (*algorithm)(int) = NULL;

    if (argc == 3)
    {
        if (strcmp(argv[1], "perm") == 0)
        {
            algorithm = perm_queen;
        }
        else
        {
            algorithm = n_queen;
        }

        int n = 0;
        if (sscanf(argv[2], "%d", &n) == 1)
        {
            algorithm(n);
        }
        else
        {
            printf("Invalid number\n");
        }
    }
    else
    {
        printf("Usage: %s [perm|back] n\n", argv[0]);
    }

    return 0;
};
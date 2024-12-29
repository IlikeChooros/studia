#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#define N_SLOTS 4
#define N_COLORS 6
#define MIN_GUESS 1111

static int *all_codes = NULL;
static int n_all_codes = 0;
static int *filtered_codes = NULL;
static int n_filtered_codes = 0;

int *generate_all()
{
    n_all_codes = pow(N_COLORS, N_SLOTS);
    all_codes = (int *)malloc(n_all_codes * sizeof(int));

    for (int i = MIN_GUESS; i < n_all_codes + MIN_GUESS; i++)
        all_codes[i - MIN_GUESS] = i;
}

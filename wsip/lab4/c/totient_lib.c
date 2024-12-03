#include "totient_lib.h"

typedef struct 
{
	int size;
	int max_size;
	int *buffer;
} array_t;

void init(array_t* arr, int max_size)
{
    arr->size = 0;
    
    if (max_size == 0)
    {
        arr->max_size = 0;
        arr->buffer = NULL;
        return;
    }

    arr->max_size = max_size;
    arr->buffer = (int*) malloc(sizeof(int) * max_size);

    if (arr->buffer == NULL)
        arr->max_size = 0;
}

void append(array_t* arr, int n)
{
    if (arr->size >= arr->max_size)
        return;
    arr->buffer[arr->size++] = n;
}

void free_array(array_t* arr)
{
    free(arr->buffer);
    arr->buffer = NULL;
    arr->size = 0;
    arr->max_size = 0;
}

// Funkcja zwraca tablicę z rozkładem liczby 'n' na czynniki pierwsze
array_t prime_factors(int n)
{
	array_t arr;

	// Nieprawidłowa liczba
	if (n <= 0)
	{
		init(&arr, 0);
		return arr;
	}

	int div = 2;
	init(&arr, (int)log2f(n));

	// Sprawdzanie podzielności przez 2
	while (n > 1 && n % div == 0)
	{
		append(&arr, div);
		n /= div;
	};

	// Sprawdzanie podzielności przez liczby nieparzyste
	int root = (int)sqrt(n);
	div = 3;
	while (n > 1 && div <= root)
	{
		if (n % div == 0)
		{
			n /= div;
			append(&arr, div);
			root = (int)sqrt(n);
		}
		else
		{
			div += 2;
		}
	}

	// Jeśli liczba 'n' jest liczbą pierwszą
	if (n > 1)
		append(&arr, n);

	return arr;
}

// Oblicza funkcję Eulera dla liczby 'n' (phi(n))
num_t totient(num_t n)
{
    if (n == 0)
        return 0;
    if (n == 1)
        return 1;
    
    array_t arr    = prime_factors(n);
    num_t result   = 1;
    int occurances = 1;

    for (int i = 0; i < arr.size; i++)
    {
        // Zliczanie wystąpień (sprawdzanie czy kolejny element jest taki sam)
        if (i + 1 < arr.size && arr.buffer[i] == arr.buffer[i + 1])
        {
            occurances++;
            continue;
        }

        // Produkt Eulera
        result *= (num_t)powf(arr.buffer[i], occurances - 1) * (arr.buffer[i] - 1);
        occurances = 1;
    }

    free_array(&arr);
    return result;
}
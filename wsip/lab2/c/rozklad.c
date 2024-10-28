#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Własna struktura tablicy
typedef struct 
{
	int size;
	int max_size;
	int *buffer;
} array_t;


// Inicjalizacja struktury array_t, alokuje pamięć dla buffer'a
void init(array_t* arr, int max_size)
{
	arr->size = 0;
	
	// Jeśli max_size == 0, to nie alokuj pamięci
	if (max_size == 0)
	{
		arr->max_size = 0;
		arr->buffer = NULL;
		return;
	}

	arr->max_size = max_size;
	arr->buffer = (int*) malloc(sizeof(int) * max_size);

	// Jeśli alokacja pamięci się nie powiodła
	if (arr->buffer == NULL)
		arr->max_size = 0;
}


// Dodaje element 'n' na koniec tablicy
void append(array_t* arr, int n)
{
	if (arr->size >= arr->max_size)
		return;
	arr->buffer[arr->size++] = n;
}


// Zwalnia pamięć zaalokowaną dla tablicy
void free_array(array_t* arr)
{
	free(arr->buffer);
	arr->buffer = NULL;
	arr->size = 0;
	arr->max_size = 0;
}


// Funkcja zwraca tablicę z rozkładem liczby 'n' na czynniki pierwsze
array_t prime_distribution(int n)
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


// Wypisuje liczbę w formacie potęgowym
void print_format(int n, int occurances, int first)
{
	if (!first)
		printf("*");
	if (occurances > 1)
		printf("%d^%d", n, occurances);
	else
		printf("%d", n);
}


// Wypisuje tablicę w określonym formatcie potęgowym
void print_array(array_t* arr)
{
	if (arr->size == 0)
	{
		printf("Brak czynników pierwszych\n");
		return;
	}

	int first = 1; // Jeżeli jest to pierwszy do wypisania
	int occurances = 0; // Liczba wystąpień danej liczby
	int prev = arr->buffer[0]; // Poprzednia liczba

	for(int i = 0; i < arr->size; )
	{
		// Jeżeli liczba jest taka sama jak poprzednia
		if (prev == arr->buffer[i])
		{
			occurances++; // Zwiększ liczbę wystąpień
			i++; // Przejdź do następnej liczby
		}
		else
		{
			// Wypisz poprzednią liczbę w odpowiednim formacie
			print_format(prev, occurances, first);

			// Reset zmiennych
			prev = arr->buffer[i];
			occurances = 0;
			first = 0;
		}
	}

	// Zawsze zostanie jedna liczba do wypisania
	print_format(prev, occurances, first);
	
	printf("\n");
}


int main(int argc, char* argv[])
{
	int n = 100;
	printf("Podaj liczbe: ");
	scanf("%d", &n);

	array_t arr = prime_distribution(n);
	print_array(&arr);
	free_array(&arr);

	return 0;
}


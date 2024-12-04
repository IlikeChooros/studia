#pragma once

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

typedef int elem_t;

typedef struct {
    elem_t value;
    struct node_t* next;
} node_t;

typedef node_t* node_ptr_t;

typedef struct {
    node_ptr_t head = NULL;
    node_ptr_t tail = NULL;
    size_t lenght   = 0L;
} list_t;

typedef list_t* list;

bool is_empty(list l);
elem_t pop(list l);
void append(list l, elem_t value);
void push(list l, elem_t value);

elem_t get(list l, size_t index);
void put(list l, size_t index, elem_t value);
void insert(list l, size_t index, elem_t value);
void delete(list l, size_t index);

void print(list l);
size_t lenght(list l);
void clean(list l);
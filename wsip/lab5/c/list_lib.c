#include "list_lib.h"

bool is_empty(list l)
{
    return l->lenght == 0;
}

// Create new node
node_ptr_t create_node(elem_t value)
{
    node_ptr_t node = (node_ptr_t)malloc(sizeof(node_t));
    node->next = NULL;
    node->value = value;
}

/**
 * @brief Find node at the given index
 * 1 <= index <= lenght
 */
node_ptr_t find_index(list l, size_t index)
{
    index--;
    if (index >= l->lenght)
        return NULL;
    node_ptr_t node = l->head;
    for (size_t i = 0; i < index && node != NULL; i++)
    {
        node = node->next;
    }

    return node;
}

/**
 * @brief Insert list element at the given index 
 * 1 <= index <= lenght + 1 (1 = at the beginning, lenght + 1 = at the end)
 */
void insert(list l, size_t index, elem_t value)
{
    index--;
    if (index > l->lenght)
        return;

    node_ptr_t node = l->head;
    node_ptr_t prev = node;
    node_ptr_t new_node = create_node(value);

    for (size_t i = 0; i < index && node != NULL; i++)
    {
        prev = node;
        node = node->next;
    }

    // Head of the list (no previous element)
    if (node == l->head)
    {
        new_node->next = l->head;
        l->head        = new_node;
    }
    // at the end
    else if (node == NULL)
    {
        prev->next     = new_node;
        l->tail        = new_node;
    }
    // in the middle insertion
    else
    {
        prev->next     = new_node;
        new_node->next = node;
    }
    l->lenght++;
}

/**
 * @brief Append list element at the end
 */
void append(list l, elem_t value)
{
    insert(l, lenght(l) + 1, value);
}

/**
 * @brief Push list element at the beginning
 */
void push(list l, elem_t value)
{
    insert(l, 1, value);
}

/**
 * @brief Get list element at the given index
 * 1 <= index <= lenght
 */
elem_t get(list l, size_t index)
{
    node_ptr_t node = find_index(l, index);
    if (node == NULL)
        return 0;
    
    return node->value;
}

/**
 * @brief Put new value at the given index
 */
void put(list l, size_t index, elem_t value)
{
    node_ptr_t node = find_index(l, index);
    if (node == NULL)
        return;
    
    node->value = value;
}

/**
 * @brief Delete list element at the given index
 */
void delete(list l, size_t index)
{
    index--;
    if (index >= l->lenght)
        return;

    node_ptr_t node = l->head;
    node_ptr_t prev = node;

    for (size_t i = 0; i < index && node != NULL; i++)
    {
        prev = node;
        node = node->next;
    }

    // Head of the list (no previous element)
    if (node == l->head)
    {
        l->head = node->next;
        free(node);
    }
    // at the end
    else if (node == l->tail)
    {
        prev->next = NULL;
        l->tail    = prev;
        free(node);
    }
    // in the middle deletion
    else
    {
        prev->next = node->next;
        free(node);
    }
    l->lenght--;
}

/**
 * @brief Pop last element from the list
 */
elem_t pop(list l)
{
    delete(l, lenght(l));
}

/**
 * @brief Print list elements
 */
void print(list l)
{
    node_ptr_t node = l->head;
    while (node != NULL)
    {
        printf("%d ", node->value);
        node = node->next;
    }
    printf("\n");
}

/**
 * @brief Get list lenght (by counting nodes) 
 */
size_t lenght(list l)
{
    size_t len = 0;
    node_ptr_t node = l->head;
    while (node != NULL)
    {
        len++;
        node = node->next;
    }

    return len;
}

/**
 * @brief Clean list (free all nodes)
 */
void clean(list l)
{
    node_ptr_t node = l->head;
    while (node != NULL)
    {
        node_ptr_t next = node->next;
        free(node);
        node = next;
    }
    l->head = NULL;
    l->tail = NULL;
    l->lenght = 0;
}
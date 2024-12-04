#include <stdio.h>
#include <string.h>

#include "list_lib.h"

// Output strings
#define COMMAND_STR "Command: "
#define VALUE_STR   "Value: "
#define RESULT_STR  "Result: "
#define INDEX_STR   "Index: "
#define ERROR_STR   "Error - "

// Commands
#define PRINT_CMD   "print"
#define APPEND_CMD  "append"
#define INSERT_CMD  "insert"
#define PUT_CMD     "put"
#define GET_CMD     "get"
#define DELETE_CMD  "delete"
#define POP_CMD     "pop"
#define PUSH_CMD    "push"
#define CLEAN_CMD   "clean"
#define EXIT_CMD    "exit"
#define HELP_CMD    "help"

#define N_COMMANDS  11

// Declarations
static list g_list    = NULL;
static elem_t g_value = 0;
static size_t g_index = 0;

// Command functions
void get_value()
{
    printf(VALUE_STR);
    scanf("%i", &g_value);
}

/**
 * @brief Get index from user
 * @returns true if index is valid, false otherwise
 */
bool get_index(bool is_insert)
{
    printf(INDEX_STR);
    scanf("%lu", &g_index);
    if (g_index <= (g_list->lenght + is_insert))
    {
        return true;
    }
    // Index out of bounds
    printf(ERROR_STR "bad index!\n");
    return false;
}

void result_ok()
{
    printf(RESULT_STR "OK\n");
}


// Actual commands

/**
 * @brief Print list elements
 */
void print_command()
{
    printf(RESULT_STR);
    print(g_list);
}

/**
 * @brief Append list element at the end
 */
void append_command()
{
    get_value();
    append(g_list, g_value);
    result_ok();
}

/**
 * @brief Insert list element at the given index,
 * 1 <= index <= lenght + 1 (1 = at the beginning, lenght + 1 = at the end)
 */
void insert_command()
{
    bool result = get_index(true);
    if (!result)
        return;

    get_value();
    
    insert(g_list, g_index, g_value);
    result_ok();
}

/**
 * @brief Put new value at the given index
 */
void put_command()
{
    bool result = get_index(false);
    if (!result)
        return;
    
    get_value();
    put(g_list, g_index, g_value);
    result_ok();
}

/**
 * @brief Push list element at the beginning
 */
void push_command()
{
    get_value();
    push(g_list, g_value);
    result_ok();
}

/**
 * @brief Get list element at the given index
 */
void get_command()
{
    bool result = get_index(false);
    if (!result)
        return;
    
    printf(RESULT_STR "%d\n", get(g_list, g_index));
}

/**
 * @brief Delete list element at the given index
 */
void delete_command()
{
    bool result = get_index(false);
    if (!result)
        return;
    
    delete(g_list, g_index);
    result_ok();
}

/**
 * @brief Delete last element from the list and print it
 */
void pop_command()
{
    if (is_empty(g_list))
    {
        printf(ERROR_STR "stack is empty\n");
        return;
    }

    printf(RESULT_STR "%d\n", pop(g_list));
}

/**
 * @brief Clean list (free all nodes)
 */
void clean_command()
{
    clean(g_list);
    result_ok();
}

// Quit command
void exit_command()
{
    printf("Bye!\n");
    exit(0);
}

// Help command
void help_command()
{
    printf("\nList of commands:\n");
    printf(PRINT_CMD "  - print list\n");
    printf(APPEND_CMD " - append element at the end\n");
    printf(INSERT_CMD " - insert element at the given index\n");
    printf(PUT_CMD "    - put new value at the given index\n");
    printf(PUSH_CMD "   - push element at the beginning\n");
    printf(GET_CMD "    - get element at the given index\n");
    printf(DELETE_CMD " - delete element at the given index\n");
    printf(POP_CMD "    - delete last element from the list and print it\n");
    printf(CLEAN_CMD "  - clean list\n");
    printf(EXIT_CMD "   - exit\n");
}

int main(int argc, char* argv[])
{
    g_list = create();
    
    void (*commands[N_COMMANDS])() = {
        print_command,
        append_command,
        insert_command,
        put_command,
        push_command,
        get_command,
        delete_command,
        pop_command,
        clean_command,
        exit_command,
        help_command
    };

    char* (command_names[N_COMMANDS]) = {
        PRINT_CMD,
        APPEND_CMD,
        INSERT_CMD,
        PUT_CMD,
        PUSH_CMD,
        GET_CMD,
        DELETE_CMD,
        POP_CMD,
        CLEAN_CMD,
        EXIT_CMD,
        HELP_CMD
    };

    printf("List test, type '" EXIT_CMD "' to exit\n");

    while (1)
    {
        printf(COMMAND_STR);
        char command[128];
        scanf("%s", command);
        bool found = false;

        for (int i = 0; i < N_COMMANDS; i++)
        {
            if (strcmp(command, command_names[i]) == 0)
            {
                found = true;
                commands[i]();
                break;
            }
        }

        if (!found)
        {
            printf(ERROR_STR "unknown command, type '"HELP_CMD"' for a list of possible commands\n");
        }
    }
}


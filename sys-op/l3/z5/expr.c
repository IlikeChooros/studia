#include "types.h"
#include "stat.h"
#include "user.h"
#include "fs.h"

/*

A simple parser, uses pratt parsing to evaluate
complex expressions

Allows only infix expressions eq. a + b * c
(not: a - -b * c)

*/

#define NULL ((void*)0)

#define DEBUG 0

#if DEBUG
#define DEBUG_PRINT(...) printf(__VA_ARGS__)
#else
#define DEBUG_PRINT(...)
#endif


typedef unsigned char unit8_t;

typedef enum {
    ATOM, // in this case, that's simply a number
    OP, // one of the Operations
    EOF // end of the expression
} token_types_t;

typedef struct {
    token_types_t type;
    int word;
} token_t;

typedef struct {
    token_t* tokens;
    int start_index; // The starting index of the list
    int n_tokens;
    int cap;
} lexer_t;

typedef struct {
    unit8_t left, right, ok;
} binding_power_t;

typedef struct expression_t {
    int atom_or_op;
    struct expression_t* lhs;
    struct expression_t* rhs;
} expression_t;

// Forward declerations
void deleteLexer(lexer_t*);
lexer_t* newLexer(const char*);
void lexerPushToken(lexer_t*, token_t);
binding_power_t getBindingPower(int);
expression_t* newAtomExpression(int);
expression_t* newOpExpression(int, expression_t*, expression_t*);
void addExpression(expression_t*, expression_t*);
expression_t* parseExpression(lexer_t*, unit8_t);
void deleteExpression(expression_t*);

// Function definitions

binding_power_t getBindingPower(int op) {
    if (op == '+' || op == '-') {
        return (binding_power_t){10, 11, 1};
    } else if (op == '*' || op == '/') {
        return (binding_power_t){20, 21, 1};
    } else if (op == '^') {
        return (binding_power_t){31, 30, 1}; // right associative, since a ^ b ^ c = a ^ (b ^ c)
    }
    return (binding_power_t){0, 0, 0};
}

void deleteLexer(lexer_t* lexer) {
    if (lexer == NULL) return;

    free(lexer->tokens);
    free(lexer);
}

lexer_t* newLexer(const char* input) {
    lexer_t* lexer = malloc((uint)sizeof(lexer_t));
    lexer->start_index = 0;
    lexer->n_tokens = 0;
    lexer->cap = 10;
    lexer->tokens = (token_t*)malloc((uint)sizeof(token_t)*lexer->cap);

    for (int i = 0; input[i] != '\0'; i++) {
        char c = input[i];
        if (c == ' ') continue;

        // Number parsing
        int num = 0, parsed = 0;
        while (c >= '0' && c <= '9') {
            num = num * 10 + (c - '0');
            i++;
            c = input[i];
            parsed = 1;
        }

        if (parsed) {
            lexerPushToken(lexer, (token_t){ATOM, num});
            i--; // step back one character
        }
        else lexerPushToken(lexer, (token_t){OP, c});
    }
    lexerPushToken(lexer, (token_t){EOF, 0});

    return lexer;
};

void lexerPushToken(lexer_t* lexer, token_t token) {
    if (lexer->n_tokens + 1 >= lexer->cap) {
        int newCap = lexer->cap * 2;
        token_t* newBuf = (token_t*)malloc((uint)sizeof(token_t)*newCap);
        memmove(newBuf, lexer->tokens, lexer->cap * sizeof(token_t));
        free(lexer->tokens);
        lexer->tokens = newBuf;
        lexer->cap = newCap;
    }
    lexer->tokens[lexer->n_tokens++] = token;
}

token_t lexerNext(lexer_t* lexer) {
    if (lexer->start_index >= lexer->n_tokens) {
        return (token_t){EOF, 0};
    }
    return lexer->tokens[lexer->start_index++];
}

token_t lexerPeek(lexer_t* lexer) {
    if (lexer->start_index >= lexer->n_tokens) {
        return (token_t){EOF, 0};
    }
    return lexer->tokens[lexer->start_index];
}

expression_t* newAtomExpression(int atom) {
    expression_t* expr = (expression_t*)malloc((uint)sizeof(expression_t));
    expr->atom_or_op = atom;
    expr->lhs = NULL;
    expr->rhs = NULL;
    return expr;
}

expression_t* newOpExpression(int op, expression_t* rhs, expression_t* lhs) {
    expression_t* expr = (expression_t*)malloc((uint)sizeof(expression_t));
    expr->atom_or_op = op;
    expr->rhs = rhs;
    expr->lhs = lhs;
    return expr;
}

void deleteExpression(expression_t* expr) {
    if (expr == NULL) return;

    if (expr->rhs != NULL) {
        deleteExpression(expr->rhs);
    }
    if (expr->lhs != NULL) {
        deleteExpression(expr->lhs);
    }

    free(expr);
}

expression_t* parseExpression(lexer_t* lexer, unit8_t minBp) {
    expression_t* lhs = NULL;
    token_t token = lexerNext(lexer); // consume the token
    
    if (token.type != ATOM) { 
        // first token cannot be other than atom (a number)
        DEBUG_PRINT(1, "Error: expected ATOM, got %d\n", token.word);
        return NULL;
    }

    DEBUG_PRINT(1, "A: %d\n", token.word);
    lhs = newAtomExpression(token.word);
    
    while (1) {
        token = lexerPeek(lexer);
        DEBUG_PRINT(1, "P: %d\n", token.word);
        if (token.type == EOF) {
            DEBUG_PRINT(1, "Breaking: EOF\n");
            break;
        }

        if (token.type == ATOM) {
            DEBUG_PRINT(1, "Error: unexpected ATOM\n");
            goto error;
        }

        // This is an operator, now check its binding power
        DEBUG_PRINT(1, "O: %d\n", token.word);
        binding_power_t bp = getBindingPower(token.word);
        if (bp.left < minBp) {
            DEBUG_PRINT(1, "Breaking: bp.left=%d < minBp=%d\n", bp.left, minBp);
            break;
        }

        lexerNext(lexer); // consume operator
        DEBUG_PRINT(1, "Recursing: bp.right=%d\n", bp.right);
        expression_t* rhs = parseExpression(lexer, bp.right);
        // If error arised, deallocate memory
        if (rhs == NULL) {
            goto error;
        }

        DEBUG_PRINT(1, "R: %d\n", rhs->atom_or_op);
        lhs = newOpExpression(token.word, rhs, lhs);
    }

    DEBUG_PRINT(1, "Returning lhs: %d\n", lhs->atom_or_op);

    return lhs;

error:
    deleteExpression(lhs);
    return NULL;
}

void printExpression(expression_t* expr) {
    if(expr == NULL) {
        printf(1, "NULL");
        return;
    }

    if (expr->lhs != NULL) {
        printf(1, "(");
        printExpression(expr->lhs);
    }
    switch (expr->atom_or_op)
    {
    case '+':
        printf(1, " + ");
        break;
    case '-':
        printf(1, " - ");
        break;
    case '*':
        printf(1, " * ");
        break;
    case '/':
        printf(1, " / ");
        break;
    case '^':
        printf(1, " ^ ");
        break;
    default:
        printf(1, "%d", expr->atom_or_op);
        break;
    }
    if (expr->rhs != NULL) {
        printExpression(expr->rhs);
        printf(1, ")");
    }
}

int evaluate(expression_t* expr) {
    // This is an atom
    if (expr->lhs == NULL && expr->rhs == NULL) {
        return expr->atom_or_op; // convert char to int
    }

    // This is an operation
    int left = evaluate(expr->lhs);
    int right = evaluate(expr->rhs);

    switch (expr->atom_or_op)
    {
    case '+':
        return left + right;
    case '-':
        return left - right;
    case '*':
        return left * right;
    case '/':
        return left / right;
    case '^': {
        int result = 1;
        for (int i = 0; i < right; i++) {
            result *= left;
        }
        return result;
    }
    default:
        return 0; // error
    }
}

int main(int argc, char** argv) {
    if (argc == 1) {
        printf(1, 
            "Usage: %s <n> [[+|-|/|*] <n>]]\n" \
            "Example usage: expr 2 * 2 (prints out 4)\n", 
        argv[0]);
        exit();
    }

    char expr_input[256];
    expr_input[0] = '\0';
    for (int i = 1; i < argc; i++) {
        // Append to expr_input
        int len = strlen(expr_input);
        expr_input[len] = ' ';
        expr_input[len + 1] = '\0';
        len++;
        memmove(&expr_input[len], argv[i], strlen(argv[i]) + 1);
    }

    DEBUG_PRINT(1, "Input expression: %s\n", expr_input);
    lexer_t* l = newLexer(expr_input);

#if DEBUG
    DEBUG_PRINT(1, "Lexer:\n");
    for (int i = 0; i < l->n_tokens; i++) {
        token_t* t = &l->tokens[i];
        if (t->type == ATOM) {
            DEBUG_PRINT(1, "ATOM: %d\n", t->word);
        } else if (t->type == OP) {
            binding_power_t bp = getBindingPower(t->word);
            DEBUG_PRINT(1, "OP: %d bp=(%d, %d)\n", t->word, bp.left, bp.right);
        } else {
            DEBUG_PRINT(1, "EOF\n");
        }
    }
#endif
    
    expression_t* expr = parseExpression(l, 0);

    // Print the expression
    // printExpression(expr);
    DEBUG_PRINT(1, "\n");

    // Evaluate the expression
    if (expr == NULL) {
        printf(1, "Error: invalid expression\n");
        deleteLexer(l);
        exit();
    }

    printf(1, "%d\n", evaluate(expr));

    deleteLexer(l);
    deleteExpression(expr);

    exit();
}


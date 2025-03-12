#include <iostream>

#include "pascal.hpp"

class IntegerConversionError : public std::runtime_error {
public:
    IntegerConversionError(const char* str_int) : std::runtime_error(
        std::string(str_int) + " - nieprawdiłowa dana"
    ) {}
};

/**
 * @brief Convert given string to integer
 * @throws `IntegerConversionError` if invalid
 */
int convert_to_int(char* str)
{
    int i;
    if (std::sscanf(str, "%d", &i) == 0)
        throw IntegerConversionError(str);
    return i;
}

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        std::cout << "Podaj argumenty: \n"
                  << argv[0] << " <size> <n1> ... <nk>\n";
        return EXIT_FAILURE;
    }

    int n = 0;
    try{
        n = convert_to_int(argv[1]);
    }
    catch(const IntegerConversionError& e) {
        std::cout << e.what() << '\n';
        return EXIT_FAILURE;
    }

    if (n < 0) {
        std::cout << argv[1] << " - Nieprawidłowy numer wiersza\n";
        return EXIT_FAILURE;
    }

    WierszTrojkataPascala w{n};

    for (int i = 2; i < argc; i++)
    {
        try{
            n = convert_to_int(argv[i]);
            n = w[n];
            std::cout << argv[i] << " - " << n << '\n';
        }
        catch(const std::runtime_error& e) {
            std::cout << e.what() << '\n';
        }
    }

    return EXIT_SUCCESS;
}
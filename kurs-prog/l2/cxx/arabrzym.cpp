#include "arabrzym.hpp"

#include <iostream>

int ArabRzym::M_get_arab_value(char c)
{
    switch (c)
    {
    case 'I':
        return 1;
    case 'V':
        return 5;
    case 'X':
        return 10;
    case 'L':
        return 50;
    case 'C':
        return 100;
    case 'D':
        return 500;
    case 'M':
        return 1000;
    default:
        return 0;
    }
}

/**
 * @brief Converts the roman number to an integer, assumes correctness of the argument
 */
int ArabRzym::toArab(std::string rzym)
{
    // VIII 8
    // XLIV 44
    // CMXL 940

    int arab = 0;

    for (int i = 0; i < rzym.size(); i++) 
    {

        // Get currrent number
        int now  = M_get_arab_value(rzym[i]);

        // Check if no 'next' exists
        if (i == rzym.size() - 1)
        {
            arab += now;
            break;
        }

        // Get the next number
        int next = M_get_arab_value(rzym[i + 1]);

        // If next is bigger than current number, then substract next from
        if (now < next) 
        {
            arab += next - now;
            i++;
        }

        // Just add the number
        else
        {
            arab += now;
        }
    }

    return arab;
}

/**
 * @brief Convert integer to roman number
 */
std::string ArabRzym::toRzym(int n)
{
    if (n <= 0 or n >= 4000)
        return "";
    
    // 44 = 40 + 4 = 4 * 10 + 4 = XL IV
    // 3155 = 3 * 10^3 + 1 * 10^2 + 5 * 10 + 5 = MMM C L V
    char pojedyncze[]   = {'I', 'V', 'X'};
    char dzisietne[]    = {'X', 'L', 'C'};
    char setne[]        = {'C', 'D', 'M'};

    // Convert a part of a number to roman
    auto toRzymPart = [](int n, char* roman){
        std::string ret = "";
        ret.reserve(3);

        if (n == 0)
            return ret;

        if (n <= 3)
            for (int i = 0; i < n; i++)
                ret += roman[0];
        if (n == 4)
        {
            ret += roman[0];
            ret += roman[1];
        }
        if (n >= 5 && n != 9)
            ret += roman[1];
        if (n <= 8)
            for (int i = 6; i < n; i++)
                ret += roman[0];
        else 
        {
            ret += roman[0];
            ret += roman[2];
        }
        
        return ret;
    };

    std::string arab;
    arab.reserve(9);

    int part = (n / 1000);
    for (int i = 0; i < part; i++)
        arab += 'M';

    arab += 
        toRzymPart((n / 100) % 10, setne) +
        toRzymPart((n / 10) % 10, dzisietne) +
        toRzymPart(n % 10, pojedyncze);

    return arab;
}


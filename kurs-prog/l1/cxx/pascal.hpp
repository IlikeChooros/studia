#pragma once

#include <vector>
#include <stdexcept>

class OutOfRangeException : public std::runtime_error {
public:
    OutOfRangeException(int i) : std::runtime_error(
        std::to_string(i) + " - liczba spoza zakresu"
    ) {}
};


class WierszTrojkataPascala
{
    std::vector<int> m_pascal;
public:
    WierszTrojkataPascala(int n) : m_pascal(n + 1, 0) 
    {
        M_init();
    }

    /**
     * @brief Get the ith number of pascal's triangle
     * @throws `OutOfRangeException` if i is invalid
     */
    inline int operator[](int i) {
        if (i < 0 || i >= m_pascal.size())
            throw OutOfRangeException(i);
        return m_pascal[i];
    }

private:

    inline void M_init()
    {
        m_pascal[0] = 1;

        for (int i = 1; i < m_pascal.size(); ++i)
        {
            // zmienna pomocnicza, by zachowac wartosc z poprzedniego elementu
            int prev = 1;

            for (int j = 1; j <= i; j++) 
            {
                // m_pascal[j] += m_pascal[j-1] dziala tylko dla pierwszego elementu,
                // dlatego potrzebny jest 'temp' i 'prev', by zachowac stan z przed 
                // dodawania
                int temp = m_pascal[j];
                m_pascal[j] += prev;
                prev = temp;
            }
        }
    }
};
#pragma once

#include <string>

class ArabRzym {
    static int M_get_arab_value(char c);
public:
    static int toArab(std::string rzym);
    static std::string toRzym(int n);
};
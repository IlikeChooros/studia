#pragma once

#include <string>

class ArabRzym {
    static std::string M_toRzym(int n);
    static int M_toArab(char c);
public:
    static int toArab(std::string rzym);
    static std::string toRzym(int n);
};
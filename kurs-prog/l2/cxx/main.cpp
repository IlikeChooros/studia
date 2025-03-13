#include <iostream>

#include "arabrzym.hpp"

int main(int argc, char** argv)
{
    std::cout << ArabRzym::toArab("XVII") << '\n';
    std::cout << ArabRzym::toArab("XLIV") << '\n';
    std::cout << ArabRzym::toArab("III") << '\n';

    std::cout << ArabRzym::toRzym(3999) << '\n';
    std::cout << ArabRzym::toRzym(15) << '\n';
    std::cout << ArabRzym::toRzym(3) << '\n';
    std::cout << ArabRzym::toRzym(44) << '\n';
    std::cout << ArabRzym::toRzym(17) << '\n';

    std::cout << "Test ->: " << 3999 << " = " << ArabRzym::toArab(ArabRzym::toRzym(3999)) << '\n';
    std::cout << "Test <-: " << "CCCXCIX" << " = " << ArabRzym::toRzym(ArabRzym::toArab("CCCXCIX")) << '\n';
    return 0;
}
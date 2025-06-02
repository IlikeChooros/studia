#include <iostream>
#include <map>
#include "binary_tree.hpp"

int main() {
    binary_tree<int> tree;

    tree.insert(0);
    tree.insert(1);
    tree.insert(-1);
    tree.insert(2);

    std::cout << tree.size() << '\n';
    std::cout << tree << '\n';

    tree.remove(0);
    std::cout << tree.size() << '\n';

    std::cout << tree << '\n';
    
    return 0;
}
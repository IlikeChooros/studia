#include <iostream>
#include <map>
#include "binary_tree.hpp"

int main() {
    binary_tree<int> tree;

    tree.insert(30);
    tree.insert(70);
    tree.insert(20);
    tree.insert(40);
    tree.insert(60);
    tree.insert(80);
    tree.insert(35);
    tree.insert(45);
    tree.insert(75);

    tree.draw();

    tree.remove(20);
    std::cout << "Tree structure after removing 20:" << std::endl;
    tree.draw();
    std::cout << std::endl;

    binary_tree<int> empty_tree;
    std::cout << "Empty tree structure:" << std::endl;

    std::cout << "Hello, World!" << std::endl;
    return 0;
}
#pragma once

#include <memory>
#include <stack>
#include <concepts>
#include <iostream>

/**
 * Concept to check if a type is weakly comparable (supports <, > operators).
 */
template <typename T>
concept weakly_comparable = requires(T t) {
    { t < t} -> std::convertible_to<bool>;
    { t > t} -> std::convertible_to<bool>;
};

template <typename T>
concept printable = requires(T t) {
    { std::cout << t } -> std::same_as<std::ostream&>;
};


template <typename T>
concept treelike_type = weakly_comparable<T> && printable<T>;

/*

### Binary tree

Simple binary tree implementation (with only 2 child nodes)
Supports iteration with for-range loops

*/
template <treelike_type T>
class binary_tree {
public:
    typedef T value_type;
    // forward declaration
    struct node;
    typedef node* node_ptr;
    typedef std::unique_ptr<node> unode_ptr;


    // Node structure for the binary tree
    // Each node contains a value and pointers to left and right children
    struct node {
        value_type M_data;
        unode_ptr M_left;
        unode_ptr M_right;

        node(value_type val) : M_data(val), M_left(nullptr), M_right(nullptr) {}

        node(const node& other) {
            (void)(*this = other); // Use assignment operator to copy
        }

        // Copy other node, recursively
        node& operator=(const node& other) {
            if (this != &other) {
                M_data = other.M_data;
                if (other.M_left) {
                    M_left = std::make_unique(*other.M_left);
                } else {
                    M_left.reset();
                }
                if (other.M_right) {
                    M_right = std::make_unique(*other.M_right);
                } else {
                    M_right.reset();
                }
            }
            return *this;
        }

        /**
         * Get the left node raw pointer
         */
        inline node* left() {
            return M_left.get();
        }

        /**
         * Get the right node raw pointer
         */
        inline node* right() {
            return M_right.get();
        }
        
        /**
         * Set given node as 'left' node
         */
        inline void left(node* l) {
            M_left.reset(l);
        }

        /**
         * Set givn nove as 'right' node
         */
        inline void right(node* r) {
            M_right.reset(r);
        }

        // Set left and right refrence pointers to nullptr
        void unattach() {
            (void)M_left.release();
            (void)M_right.release();
        }
    };


    // Forward iterator for binary tree
    class iterator {
    public:
        // Iterator traits (to support for-range loops)
        using iterator_category = std::forward_iterator_tag;
        using value_type = binary_tree::value_type;
        using difference_type = std::ptrdiff_t;
        using refrence = value_type&;
        using pointer = value_type*;

    private:
        friend class binary_tree<T>;

        node_ptr M_current_node;
        std::stack<node_ptr> M_path_stack;

        // Initializes a binary tree iterator
        // Finds leftmost element of this subtree
        // And sets it as the current node
        void M_init_iterator(node_ptr node) {
            M_current_node = node;

            if (M_current_node != nullptr) {
                // Find most left node from this root
                while (M_current_node->M_left != nullptr) {
                    M_path_stack.push(M_current_node);
                    M_current_node = M_current_node->M_left.get();
                }
            }
        }

        // Allow binary tree to create an 'begin' iterator
        iterator(node_ptr current) {
            M_init_iterator(current);
        }

    public:
        iterator() : iterator(nullptr) {}

        iterator(const iterator& other) {
            (*this) = other;
        }

        iterator& operator=(const iterator& other) {
            M_current_node = other.M_current_node;
            M_path_stack = other.M_path_stack;
        }

        refrence operator*() {
            return M_current_node->M_data;
        }

        refrence operator->() {
            return M_current_node->M_data;
        }

        friend bool operator==(const iterator& lhs, const iterator& rhs) noexcept {
            return lhs.M_current_node == rhs.M_current_node;
        }

        friend bool operator!=(const iterator& lhs, const iterator& rhs) noexcept {
            return lhs.M_current_node != rhs.M_current_node;
        }

        // Pre-increment operator
        iterator& operator++() {
            // Don't allow end iterator incrementation
            if (M_current_node == nullptr) {
                return *this;
            }

            // If there is a right subtree, go to the leftomst element
            if (M_current_node->right() != nullptr) {
                M_init_iterator(M_current_node->right());
            }

            // Else, go back up the path (if possible)
            else if (!M_path_stack.empty()) {
                M_current_node = M_path_stack.top();
                M_path_stack.pop();
            }

            // There is no elements to traverse, reached the end
            else {
                M_current_node = nullptr;
            }

            return *this;
        }

        // Post-incremeent operator
        iterator operator++(int) {
            auto temp = (*this);
            ++(*this);
            return temp;
        }
    };


protected:
    unode_ptr M_root;
    size_t M_size;

    typedef struct {
        node_ptr elem;
        node_ptr parent;
    } find_helper_t;


    // Helper function to insert a value into the binary tree
    void M_insert(unode_ptr* root, const value_type& val) noexcept
    {
        // Iterative approach
        unode_ptr* curr = root;

        // While pointer inside the curr is not null,
        // traverse the tree to find the correct position for the new value
        while(*curr != nullptr) {
            if (val < (*curr)->M_data) {
                curr = &(*curr)->M_left;
            } else if (val > (*curr)->M_data) {
                curr = &(*curr)->M_right;
            } else {
                return;
            }
        }

        *curr = std::make_unique<node>(val);
        M_size++;
    }

    // Helper function to find a node with the given value
    find_helper_t M_find(const value_type& val) noexcept {
        find_helper_t result = {M_root.get(), nullptr};

        while (result.elem != nullptr) {
            if (val < (result.elem)->M_data) {
                result.parent = result.elem;
                result.elem = (result.elem)->left();
            } else if (val > (result.elem)->M_data) {
                result.parent = result.elem;
                result.elem = (result.elem)->right();
            } else {
                // Value found, return the pointer to the node
                break;
            }
        }

        return result;
    }

    /**
     * Get the rightmost element starting from given `root`
     */
    static node_ptr M_get_max(node_ptr root) {
        if (root == nullptr) {
            return nullptr;
        }

        // Traverse through the nodes, get the rightmost element
        while (root->right() != nullptr) {
            root = root->right();
        }
        return root;
    }

    /**
     * Get the leftmost element, starting from given `root`
     */
    static node_ptr M_get_min(node_ptr root) {
        if (root == nullptr) {
            return nullptr;
        }

        while (root->left() != nullptr) {
            root = root->left();
        }
        return root;
    }

    // Remove given node from the tree
    void M_remove(find_helper_t rmv) noexcept {
        if (rmv.elem == nullptr) {
            return;
        }

        // set up variables, and get the children count
        node_ptr to_remove = rmv.elem;
        node_ptr parent    = rmv.parent;
        node_ptr newchild  = nullptr;
        int children_count = 
                (int)(to_remove->left()  != nullptr) 
              + (int)(to_remove->right() != nullptr);

        bool isLeftChild = (parent != nullptr) && (parent->left() == to_remove);


        // switch by number of children (of element we want to remove)
        switch (children_count)
        {
        case 0:
            // No children, just remove the element
            break;
        case 1: {
            // there is 1 child, make it our 'newchild'
            if (to_remove->left() != nullptr) {
                newchild = to_remove->left();
            }
            else {
                newchild = to_remove->right();
            }
        }
            break;
        
        case 2: {
            // There are 2 subtree's, check if we are on
            // the left, if so, then attach right child as newchild
            // and get the leftmost leaf (from newchild's subtree)
            // then attach left subtree to the leftmost leaf.
            // Similarly, if we are on the right.
            if (isLeftChild) {
                // Make our right child 'newchild'
                // then append our left child, to newchild's leftmost element
                newchild = to_remove->right();
                node_ptr leftmost = M_get_min(newchild);
                leftmost->left(to_remove->left());
            }
            else {
                // Take our left child, and make it 'newchild',
                // then append our right child to newchild's rightmost element
                newchild = to_remove->left();
                node_ptr rightmost = M_get_max(newchild);
                rightmost->right(to_remove->right());
            }
        }
            break;
        }

        // unattach 'left' and 'right' subtrees
        to_remove->unattach();

        // attach our new subtree to the parent
        if (parent != nullptr) {
            if (isLeftChild) {
                // Kill call destructor on the previous node
                // (to_remove) and set new value
                parent->M_left.reset(newchild);
            }
            else {
                parent->M_right.reset(newchild);
            }
        }
        else {
            // that means to_remove == M_root
            // so set the root again
            M_root.reset(newchild);
        }
    }



    // ------------------ STANDARD OUTPUT ------------------
 
    /**
     * Recursively draws the tree structure.
     * @param current_node The current node to print.
     * @param prefix A string prefix showing the branch structure.
     * @param is_left True if current_node is a left child.
     */
    void M_draw_recursive(node_ptr current_node) const noexcept {
        if (current_node == nullptr) {
            std::cout << "()";
            return; 
        }
        
        std::cout << "(" << current_node->M_data << ":";
        M_draw_recursive(current_node->left());        
        std::cout << ":";
        M_draw_recursive(current_node->right());
        std::cout << ")";
    }

public:
    binary_tree() : M_root(nullptr), M_size(0) {}

    /** 
     * Inserts a value into the binary tree
     * If the value already exists, it will not be inserted again.
     * @param val The value to be inserted into the tree.
     */
    inline void insert(const value_type& val) noexcept {
        M_insert(&M_root, val);
    }

    /**
     * Search the binary tree for given value,
     * if found returns true
     */
    inline bool search(const value_type& val) noexcept {
        return *M_find(val) != nullptr;
    }

    /**
     * Remove given value from the tree
     */
    inline bool remove(const value_type& val) noexcept {
        find_helper_t f = M_find(val);

        if (f.elem == nullptr) {
            return false;
        }

        M_remove(f);
        return true;
    }

    /**
     * Draws the structure of the binary tree to standard output.
     */
    void draw() const noexcept {
        if (M_root == nullptr) {
            std::cout << "(empty tree)\n" << std::endl;
            return;
        }

        M_draw_recursive(M_root.get());
        std::cout << '\n';
    }

    // Returns actual size of the tree (number of nodes)
    inline size_t size() {
        return M_size;
    }

    // Returns forward iterator pointing to
    // leftmost element in the tree (the smallest)
    inline iterator begin() noexcept {
        return iterator(M_root.get());
    }

    // Returns end iterator (pointing to nullpointer as value)
    inline iterator end() noexcept {
        return iterator();
    }
};

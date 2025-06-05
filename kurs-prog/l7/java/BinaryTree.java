import java.util.Iterator;
import java.util.Stack;

/**
 * Binary tree implementation, with iterator
 */
public class BinaryTree<E extends Comparable<E>> implements Iterable<E>{

    /**
     * Node structure for the tree, has left and right pointers
     */
    private class Node implements Cloneable {
        Node left;
        Node right;
        E value;

        public Node(E value) {
            this.value = value;
        }

        public void unattach() {
            left = null;
            right = null;
        }
    }

    /**
     * Iterator for BinaryTree, goes from minimal to maximal element
     */
    private class BinaryIterator implements Iterator<E> {
        private Node current = null;
        private Node prev = null;
        private Stack<Node> path_stack = new Stack<>();

        /**
         * Get the leftmost node from given root
         * @param root
         */
        private void leftmost(Node root) {
            current = root;

            if (current != null) {
                while (current.left != null) {
                    path_stack.push(current);
                    current = current.left;
                }
            }
        }

        public BinaryIterator(Node root) {
            leftmost(root);
        }

        @Override
        public boolean hasNext() {
            // Dont' allow end interator incrementation
            prev = current;
            if (prev == null) {
                return false;
            }

            // If there is right subtree, go to
            // leftmos element
            if (current.right != null) {
                leftmost(current.right);
            }
            else if (!path_stack.isEmpty()) {
                current = path_stack.pop();
            }
            else {
                current = null;
            }

            return true;
        }

        @Override
        public E next() {
            return prev.value;
        }
    }

    private Node root;
    private int size;

    /**
     * Class to find given value in the tree, starting from 'elem'
     */
    private class FindHelper {
        public Node elem;
        public Node parent;

        public FindHelper(Node elem, Node parent) {
            this.elem = elem;
            this.parent = parent;
        }

        public FindHelper(Node elem, Node parent, E value) {
            this.elem = elem;
            this.parent = parent;
            this.find(value);
        }

        public Node find(E value) {
            while (this.elem != null) {
                int cmp = value.compareTo(this.elem.value);

                if (cmp < 0) {
                    this.parent = this.elem;
                    this.elem = this.elem.left;
                } else if (cmp > 0) {
                    this.parent = this.elem;
                    this.elem = this.elem.right;
                } else {
                    // Value found
                    break;
                }
            }
            return this.elem;
        }
    }

    /**
     * Insertion helper, start's from given root and attaches new node
     * with given value, while preserving BTS structure
     * In optimistic O(log(n)) time (worst-case O(n))
     */
    private void insert_helper(Node root, E value) {

        size++;

        if (this.root == null) {
            this.root = new Node(value);
            return;
        }
        
        Node curr = root;
        Node prev = null; 

        while (curr != null) {
            int cmpResult = value.compareTo(curr.value);
            
            // value < curr.value
            if (cmpResult < 0) {
                prev = curr;
                curr = curr.left;
            }
            else if (cmpResult > 0) {
                prev = curr;
                curr = curr.right;
            } else {
                return;
            }
        }

        int cmpResult = value.compareTo(prev.value);
        if (cmpResult < 0) {
            prev.left = new Node(value);
        } else {
            prev.right = new Node(value);
        }
    }

    /**
     * Helper method to get maximum node (rightmost elem of the tree)
     */
    private Node get_max_node(Node root) {
        if (root == null) {
            return null;
        }

        while (root.right != null) {
            root = root.right;
        }
        return root;
    }

    /**
     * Helper method to get the minimum node of the tree (leftmost)
     */    
    private Node get_min_node(Node root) {
        if (root == null) {
            return null;
        }

        while (root.left != null) {
            root = root.left;
        }
        return root;
    }

    /**
     * Helper method to delete an element
     * @param rmv FindHelper result (we need to know the parent of this node)
     */
    private void remove_helper(FindHelper rmv) {
        if (rmv.elem == null) {
            return;
        }

        this.size--;
        Node to_remove = rmv.elem;
        Node parent = rmv.parent;
        Node newchild = null;
        boolean isLeftChild = (parent != null) && (parent.left == to_remove);

        // 2 children
        if (to_remove.left != null && to_remove.right != null) {
            if (isLeftChild) {
                newchild = to_remove.right;
                Node leftmost = get_min_node(newchild);
                leftmost.left = to_remove.left;
            } else {
                newchild = to_remove.left;
                Node rightmost = get_max_node(newchild);
                rightmost.right = to_remove.right;
            }
        } 
        else if (to_remove.left != null || to_remove.right != null){
            if (to_remove.left != null) {
                newchild = to_remove.left;
            }
            else {
                newchild = to_remove.right;
            }
        }

        to_remove.unattach();

        if (parent != null) {
            // Attach the subtree again to the parent
            if (isLeftChild) {
                parent.left = newchild;
            }
            else {
                parent.right = newchild;
            }
        }
        else {
            // That means rmv.elem == root
            // So set new root
            this.root = newchild;
        }
    }

    private String to_string_impl(Node root) {
        if (root != null) {
            return "(" + root.value + ":" + to_string_impl(root.left) + ":" + to_string_impl(root.right) + ")";
        } else {
            return "()";
        }
    }

    // ----------- PUBLIC -----------
    public BinaryTree() {
        root = null;
        size = 0;
    }

    /**
     * Get total number of nodes in the tree
     */
    public int size() {
        return this.size;
    }


    /**
     * Insert given value to the tree 
     * @param value
     */
    public void insert(E value) {
        insert_helper(root, value);
    }

    /**
     * Returns true if given falue exists in the tree
     */
    public boolean search(E value) {
        return (new FindHelper(root, null)).find(value) != null;
    }

    /**
     * Get the maximum element from the tree (might be null if empty)
     */
    public E max() {
        Node max_node = get_max_node(root);

        if (max_node != null) {
            return max_node.value;
        }

        return null;
    }

    /**
     * Get the minimum element from the tree (might be null if empty)
     */
    public E min() {
        Node min_node = get_min_node(root);

        if (min_node != null) {
            return min_node.value;
        }

        return null;
    }

    /**
     * Deletes given value from the tree
     * @param value to delete
     * @return true if it was deleted
     */
    public boolean delete(E value) {
        FindHelper f = new FindHelper(root, null, value);

        if (f.elem == null) {
            return false;
        }

        remove_helper(f);
        return true;
    }

    /**
     * Print this tree ot standard output stream
     */
    public void draw() {
        System.out.println(toString());
    }


    @Override
    public String toString() {
        return "BinaryTree: " + to_string_impl(root);
    }

    @Override
    public Iterator<E> iterator() {
        return new BinaryIterator(root);
    }
}   

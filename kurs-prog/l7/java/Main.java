public class Main {
    public static void main(String[] args) {

        BinaryTree<Integer> tree = new BinaryTree<>();
        tree.insert(2);
        tree.insert(25);
        tree.insert(0);
        tree.insert(100);

        for (Integer s : tree) {
            System.out.println(s);
        }

        System.out.println(tree);
    }
}
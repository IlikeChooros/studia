public class Main {
    public static void main(String[] args) {

        BinaryTree<Integer> tree = new BinaryTree<>();
        tree.insert(2);
        tree.insert(25);
        tree.insert(21);
        tree.insert(0);
        tree.insert(100);

        System.out.println(tree.size());
        System.out.println(tree.search(0));
        System.out.println(tree.search(100));
        System.out.println(tree.search(-1));

        tree.draw();

        System.out.println(tree.delete(2));
        System.out.println(tree.delete(25));
        System.out.println(tree.size());

        tree.draw();

        for (Integer s : tree) {
            System.out.println(s);
        }
    }
}
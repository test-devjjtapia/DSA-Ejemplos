package data_structures;

import java.util.ArrayList;
import java.util.List;

public class BinarySearchTreeDemo {
    TreeNode root;

    public BinarySearchTreeDemo() {
        this.root = null;
    }

    public void insert(int key) {
        root = insertRecursive(root, key);
    }

    private TreeNode insertRecursive(TreeNode current, int key) {
        if (current == null) {
            return new TreeNode(key);
        }

        if (key < current.key) {
            current.left = insertRecursive(current.left, key);
        } else if (key > current.key) {
            current.right = insertRecursive(current.right, key);
        } else {
            // value already exists
            return current;
        }
        return current;
    }

    public boolean search(int key) {
        return searchRecursive(root, key);
    }

    private boolean searchRecursive(TreeNode current, int key) {
        if (current == null) {
            return false;
        }
        if (key == current.key) {
            return true;
        }
        return key < current.key
          ? searchRecursive(current.left, key)
          : searchRecursive(current.right, key);
    }

    public void inorderTraversal() {
        List<Integer> result = new ArrayList<>();
        inorderTraversalRecursive(root, result);
        System.out.println(result);
    }

    private void inorderTraversalRecursive(TreeNode node, List<Integer> result) {
        if (node != null) {
            inorderTraversalRecursive(node.left, result);
            result.add(node.key);
            inorderTraversalRecursive(node.right, result);
        }
    }

    public static void main(String[] args) {
        BinarySearchTreeDemo bst = new BinarySearchTreeDemo();
        System.out.println("--- Insertando elementos en el BST ---");
        int[] keys = {50, 30, 20, 40, 70, 60, 80};
        for (int key : keys) {
            bst.insert(key);
            System.out.println("Insertado: " + key);
        }

        System.out.println("\n--- Búsquedas en el BST ---");
        int searchKey = 40;
        if (bst.search(searchKey)) {
            System.out.println("Clave " + searchKey + " encontrada.");
        } else {
            System.out.println("Clave " + searchKey + " no encontrada.");
        }

        searchKey = 90;
        if (bst.search(searchKey)) {
            System.out.println("Clave " + searchKey + " encontrada.");
        } else {
            System.out.println("Clave " + searchKey + " no encontrada.");
        }

        System.out.println("\n--- Recorrido In-Order (debe salir ordenado) ---");
        bst.inorderTraversal();
    }
}

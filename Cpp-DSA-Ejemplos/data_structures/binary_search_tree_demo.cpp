#include <iostream>
#include <vector>
#include <string>

// Definición de un Nodo del Árbol
struct TreeNode {
    int key;
    TreeNode* left;  // Puntero al hijo izquierdo
    TreeNode* right; // Puntero al hijo derecho

    // Constructor
    TreeNode(int val) : key(val), left(nullptr), right(nullptr) {}
};

// Definición del Árbol de Búsqueda Binaria
class BinarySearchTree {
private:
    TreeNode* root; // Puntero a la raíz del árbol

    // Función auxiliar recursiva para insertar
    TreeNode* insertRecursive(TreeNode* current, int key) {
        if (current == nullptr) {
            return new TreeNode(key);
        }

        if (key < current->key) {
            current->left = insertRecursive(current->left, key);
        } else if (key > current->key) {
            current->right = insertRecursive(current->right, key);
        } else {
            // El valor ya existe, no se inserta duplicado
            return current;
        }
        return current;
    }

    // Función auxiliar recursiva para buscar
    bool searchRecursive(TreeNode* current, int key) const {
        if (current == nullptr) {
            return false;
        }
        if (key == current->key) {
            return true;
        }
        if (key < current->key) {
            return searchRecursive(current->left, key);
        } else {
            return searchRecursive(current->right, key);
        }
    }

    // Función auxiliar recursiva para el recorrido In-Order
    void inorderTraversalRecursive(TreeNode* node, std::vector<int>& result) const {
        if (node != nullptr) {
            inorderTraversalRecursive(node->left, result);
            result.push_back(node->key);
            inorderTraversalRecursive(node->right, result);
        }
    }

    // Función auxiliar recursiva para liberar memoria (post-order traversal)
    void deleteTree(TreeNode* node) {
        if (node != nullptr) {
            deleteTree(node->left);
            deleteTree(node->right);
            delete node;
        }
    }

public:
    // Constructor
    BinarySearchTree() : root(nullptr) {}

    // Destructor para liberar toda la memoria del árbol
    ~BinarySearchTree() {
        deleteTree(root);
    }

    // Inserta una nueva clave en el BST
    void insert(int key) {
        root = insertRecursive(root, key);
    }

    // Busca una clave en el BST
    bool search(int key) const {
        return searchRecursive(root, key);
    }

    // Realiza un recorrido In-Order y retorna los elementos en un vector
    std::vector<int> inorderTraversal() const {
        std::vector<int> result;
        inorderTraversalRecursive(root, result);
        return result;
    }
};

// Función de demostración para el BST
void binary_search_tree_demo() {
    BinarySearchTree bst;
    std::cout << "--- Insertando elementos en el BST ---" << std::endl;
    std::vector<int> keys = {50, 30, 20, 40, 70, 60, 80};
    for (int key : keys) {
        bst.insert(key);
        std::cout << "Insertado: " << key << std::endl;
    }

    std::cout << "\n--- Búsquedas en el BST ---" << std::endl;
    int search_key = 40;
    if (bst.search(search_key)) {
        std::cout << "Clave " << search_key << " encontrada." << std::endl;
    } else {
        std::cout << "Clave " << search_key << " no encontrada." << std::endl;
    }

    search_key = 90;
    if (bst.search(search_key)) {
        std::cout << "Clave " << search_key << " encontrada." << std::endl;
    } else {
        std::cout << "Clave " << search_key << " no encontrada." << std::endl;
    }

    std::cout << "\n--- Recorrido In-Order (debe salir ordenado) ---" << std::endl;
    std::vector<int> inorder_result = bst.inorderTraversal();
    for (int val : inorder_result) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
}

int main() {
    binary_search_tree_demo();
    return 0;
}

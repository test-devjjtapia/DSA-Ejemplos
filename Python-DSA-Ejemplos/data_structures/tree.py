"""

Implementación de un Árbol de Búsqueda Binaria (BST - Binary Search Tree).

Un BST es una estructura de datos de árbol basada en nodos donde cada nodo tiene un
valor comparable y dos subárboles, el izquierdo y el derecho. 

Propiedades:
1. El subárbol izquierdo de un nodo contiene solo nodos con valores menores que el valor del nodo.
2. El subárbol derecho de un nodo contiene solo nodos con valores mayores que el valor del nodo.
3. Ambos subárboles izquierdo y derecho también deben ser árboles de búsqueda binaria.
"""

class Node:
    """Nodo de un árbol binario."""
    def __init__(self, key):
        self.left = None
        self.right = None
        self.val = key

class BinarySearchTree:
    """Clase para el Árbol de Búsqueda Binaria."""
    def __init__(self):
        self.root = None

    def insert(self, key):
        """Inserta una nueva clave en el BST."""
        if self.root is None:
            self.root = Node(key)
        else:
            self._insert_recursive(self.root, key)

    def _insert_recursive(self, current_node, key):
        if key < current_node.val:
            if current_node.left is None:
                current_node.left = Node(key)
            else:
                self._insert_recursive(current_node.left, key)
        else:
            if current_node.right is None:
                current_node.right = Node(key)
            else:
                self._insert_recursive(current_node.right, key)

    def search(self, key):
        """Busca una clave en el BST."""
        return self._search_recursive(self.root, key)

    def _search_recursive(self, current_node, key):
        if current_node is None or current_node.val == key:
            return current_node
        if key < current_node.val:
            return self._search_recursive(current_node.left, key)
        return self._search_recursive(current_node.right, key)

    def inorder_traversal(self, node, result):
        """Recorrido In-Order (Izquierda, Raíz, Derecha) -> Ordenado."""
        if node:
            self.inorder_traversal(node.left, result)
            result.append(node.val)
            self.inorder_traversal(node.right, result)

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    bst = BinarySearchTree()
    print("--- Insertando elementos en el BST ---")
    keys = [50, 30, 20, 40, 70, 60, 80]
    for key in keys:
        bst.insert(key)
        print(f"Insertado: {key}")

    print("\n--- Búsquedas en el BST ---")
    search_key = 40
    if bst.search(search_key):
        print(f"Clave {search_key} encontrada.")
    else:
        print(f"Clave {search_key} no encontrada.")

    search_key = 90
    if bst.search(search_key):
        print(f"Clave {search_key} encontrada.")
    else:
        print(f"Clave {search_key} no encontrada.")

    print("\n--- Recorrido In-Order (debe salir ordenado) ---")
    inorder_result = []
    bst.inorder_traversal(bst.root, inorder_result)
    print(inorder_result)

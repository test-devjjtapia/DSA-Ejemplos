"""
Implementación de un Trie (Árbol de Prefijos).

Un Trie es una estructura de datos de árbol utilizada para almacenar una colección
dinámica de cadenas, como las palabras de un diccionario. Permite búsquedas
rápidas de palabras y prefijos.

Cada nodo del Trie representa un solo carácter. Una palabra se forma siguiendo
un camino desde la raíz hasta un nodo marcado como fin de palabra.
"""

class TrieNode:
    """Un nodo en la estructura del Trie."""
    def __init__(self):
        # Diccionario para almacenar los nodos hijos
        self.children = {}
        # Booleano para marcar si este nodo es el final de una palabra
        self.is_end_of_word = False

class Trie:
    """Clase que representa el Trie completo."""
    def __init__(self):
        """Inicializa el Trie con un nodo raíz vacío."""
        self.root = TrieNode()

    def insert(self, word):
        """Inserta una palabra en el Trie."""
        current = self.root
        for char in word:
            # Si el carácter no está en los hijos del nodo actual, créalo
            if char not in current.children:
                current.children[char] = TrieNode()
            current = current.children[char]
        # Marcar el último nodo como el final de la palabra
        current.is_end_of_word = True

    def search(self, word):
        """Busca si una palabra completa existe в el Trie."""
        current = self.root
        for char in word:
            if char not in current.children:
                return False
            current = current.children[char]
        return current.is_end_of_word

    def starts_with(self, prefix):
        """Comprueba si hay alguna palabra en el Trie que comience con el prefijo dado."""
        current = self.root
        for char in prefix:
            if char not in current.children:
                return False
            current = current.children[char]
        return True

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    trie = Trie()

    print("--- Insertando palabras ---")
    palabras = ["casa", "casco", "castillo", "gato", "gata"]
    for palabra in palabras:
        trie.insert(palabra)
        print(f"Insertado: '{palabra}'")

    print("\n--- Buscando palabras completas ---")
    print(f"¿Existe 'casa'? {trie.search('casa')}")       # True
    print(f"¿Existe 'cas'? {trie.search('cas')}")         # False (es un prefijo, no una palabra completa)
    print(f"¿Existe 'castillo'? {trie.search('castillo')}") # True
    print(f"¿Existe 'casco'? {trie.search('casco')}")     # True
    print(f"¿Existe 'perro'? {trie.search('perro')}")     # False

    print("\n--- Buscando prefijos ---")
    print(f"¿Hay palabras que empiezan con 'cas'? {trie.starts_with('cas')}") # True
    print(f"¿Hay palabras que empiezan con 'ga'? {trie.starts_with('ga')}")   # True
    print(f"¿Hay palabras que empiezan con 'cat'? {trie.starts_with('cat')}") # False
    print(f"¿Hay palabras que empiezan con 'casta'? {trie.starts_with('casta')}") # True (por 'castillo')

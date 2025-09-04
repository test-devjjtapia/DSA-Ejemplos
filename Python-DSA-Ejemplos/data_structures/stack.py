"""

Implementación de una Pila (Stack).

Una pila es una estructura de datos LIFO (Last-In, First-Out),
lo que significa que el último elemento añadido es el primero en ser eliminado.

Se asemeja a una pila de platos: solo puedes tomar el plato de arriba.
"""

class Stack:
    """
    Implementación simple de una Pila utilizando una lista de Python.
    """
    def __init__(self):
        """Inicializa una pila vacía."""
        self._items = []

    def is_empty(self):
        """
        Comprueba si la pila está vacía.
        Retorna True si está vacía, False en caso contrario.
        """
        return not bool(self._items)

    def push(self, item):
        """
        Añade un elemento a la cima de la pila.
        """
        self._items.append(item)

    def pop(self):
        """
        Elimina y retorna el elemento de la cima de la pila.
        Retorna None si la pila está vacía.
        """
        if not self.is_empty():
            return self._items.pop()
        print("Error: La pila está vacía.")
        return None

    def peek(self):
        """
        Retorna el elemento de la cima de la pila sin eliminarlo.
        Retorna None si la pila está vacía.
        """
        if not self.is_empty():
            return self._items[-1]
        return None

    def size(self):
        """
        Retorna el número de elementos en la pila.
        """
        return len(self._items)

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    pila = Stack()

    print(f"¿La pila está vacía? {pila.is_empty()}")

    print("\n--- Añadiendo elementos (push) ---")
    pila.push("Libro 1")
    pila.push("Libro 2")
    pila.push("Libro 3")
    print(f"Pila actual: {pila._items}")
    print(f"Elemento en la cima (peek): {pila.peek()}")
    print(f"Tamaño de la pila: {pila.size()}")

    print("\n--- Eliminando elementos (pop) ---")
    elemento_quitado = pila.pop()
    print(f"Elemento quitado: {elemento_quitado}")
    print(f"Pila actual: {pila._items}")

    elemento_quitado = pila.pop()
    print(f"Elemento quitado: {elemento_quitado}")
    print(f"Pila actual: {pila._items}")

    print(f"\nElemento en la cima ahora: {pila.peek()}")
    print(f"¿La pila está vacía? {pila.is_empty()}")

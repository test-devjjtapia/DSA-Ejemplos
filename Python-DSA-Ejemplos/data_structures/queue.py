"""
Implementación de una Cola (Queue).

Una cola es una estructura de datos FIFO (First-In, First-Out),
lo que significa que el primer elemento añadido es el primero en ser eliminado.

Se asemeja a una fila de personas: la primera persona que llega es la primera en ser atendida.
"""
from collections import deque

class Queue:
    """
    Implementación eficiente de una Cola utilizando `collections.deque`.
    `deque` está optimizado para inserciones y eliminaciones rápidas en ambos extremos.
    """
    def __init__(self):
        """Inicializa una cola vacía."""
        self._items = deque()

    def is_empty(self):
        """
        Comprueba si la cola está vacía.
        Retorna True si está vacía, False en caso contrario.
        """
        return not bool(self._items)

    def enqueue(self, item):
        """
        Añade un elemento al final de la cola.
        """
        self._items.append(item)

    def dequeue(self):
        """
        Elimina y retorna el elemento del frente de la cola.
        Retorna None si la cola está vacía.
        """
        if not self.is_empty():
            return self._items.popleft()
        print("Error: La cola está vacía.")
        return None

    def peek(self):
        """
        Retorna el elemento del frente de la cola sin eliminarlo.
        Retorna None si la cola está vacía.
        """
        if not self.is_empty():
            return self._items[0]
        return None

    def size(self):
        """
        Retorna el número de elementos en la cola.
        """
        return len(self._items)

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    cola = Queue()

    print(f"¿La cola está vacía? {cola.is_empty()}")

    print("\n--- Añadiendo elementos (enqueue) ---")
    cola.enqueue("Cliente A")
    cola.enqueue("Cliente B")
    cola.enqueue("Cliente C")
    print(f"Cola actual: {list(cola._items)}")
    print(f"Elemento en el frente (peek): {cola.peek()}")
    print(f"Tamaño de la cola: {cola.size()}")

    print("\n--- Atendiendo elementos (dequeue) ---")
    cliente_atendido = cola.dequeue()
    print(f"Cliente atendido: {cliente_atendido}")
    print(f"Cola actual: {list(cola._items)}")

    cliente_atendido = cola.dequeue()
    print(f"Cliente atendido: {cliente_atendido}")
    print(f"Cola actual: {list(cola._items)}")

    print(f"\nPróximo cliente a atender: {cola.peek()}")
    print(f"¿La cola está vacía? {cola.is_empty()}")

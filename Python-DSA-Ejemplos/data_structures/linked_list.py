"""

Implementación de una Lista Enlazada (Singly Linked List).

Una lista enlazada es una estructura de datos lineal donde los elementos no se almacenan
en ubicaciones de memoria contiguas. Consiste en nodos, donde cada nodo contiene
un dato y una referencia (o puntero) al siguiente nodo en la secuencia.
"""

class Node:
    """Un nodo individual de una lista enlazada."""
    def __init__(self, data):
        self.data = data
        self.next = None

class LinkedList:
    """Clase que representa la lista enlazada completa."""
    def __init__(self):
        """Inicializa una lista vacía."""
        self.head = None

    def is_empty(self):
        """Comprueba si la lista está vacía."""
        return self.head is None

    def append(self, data):
        """Añade un nodo al final de la lista."""
        new_node = Node(data)
        if self.is_empty():
            self.head = new_node
            return
        
        last_node = self.head
        while last_node.next:
            last_node = last_node.next
        last_node.next = new_node

    def prepend(self, data):
        """Añade un nodo al principio de la lista."""
        new_node = Node(data)
        new_node.next = self.head
        self.head = new_node

    def delete(self, data):
        """Elimina la primera ocurrencia de un nodo con el dato especificado."""
        if self.is_empty():
            return

        if self.head.data == data:
            self.head = self.head.next
            return

        current_node = self.head
        while current_node.next and current_node.next.data != data:
            current_node = current_node.next
        
        if current_node.next:
            current_node.next = current_node.next.next
        else:
            print(f"Dato '{data}' no encontrado en la lista.")

    def display(self):
        """Muestra los elementos de la lista."""
        elements = []
        current_node = self.head
        while current_node:
            elements.append(str(current_node.data))
            current_node = current_node.next
        print(" -> ".join(elements) + " -> None")

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    lista = LinkedList()
    print(f"¿Lista vacía? {lista.is_empty()}")

    print("\n--- Añadiendo elementos (append) ---")
    lista.append("A")
    lista.append("B")
    lista.append("C")
    lista.display()

    print("\n--- Añadiendo al inicio (prepend) ---")
    lista.prepend("Inicio")
    lista.display()
    print(f"¿Lista vacía? {lista.is_empty()}")

    print("\n--- Eliminando elementos ---")
    lista.delete("B")
    print("Después de eliminar 'B':")
    lista.display()

    lista.delete("Inicio")
    print("Después de eliminar 'Inicio':")
    lista.display()

    lista.delete("Z") # Intentar eliminar un elemento que no existe

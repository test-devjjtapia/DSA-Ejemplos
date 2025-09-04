#include <iostream>
#include <string>

// Definición de un Nodo
struct Node {
    std::string data;
    Node* next; // Puntero al siguiente nodo

    // Constructor
    Node(std::string val) : data(val), next(nullptr) {}
};

// Definición de la Lista Enlazada
class LinkedList {
private:
    Node* head; // Puntero al primer nodo de la lista

public:
    // Constructor
    LinkedList() : head(nullptr) {}

    // Destructor para liberar la memoria
    ~LinkedList() {
        Node* current = head;
        while (current != nullptr) {
            Node* next_node = current->next;
            delete current;
            current = next_node;
        }
        head = nullptr;
    }

    // Comprueba si la lista está vacía
    bool isEmpty() const {
        return head == nullptr;
    }

    // Añade un nodo al final de la lista
    void append(std::string data) {
        Node* new_node = new Node(data);
        if (isEmpty()) {
            head = new_node;
            return;
        }

        Node* current = head;
        while (current->next != nullptr) {
            current = current->next;
        }
        current->next = new_node;
    }

    // Añade un nodo al principio de la lista
    void prepend(std::string data) {
        Node* new_node = new Node(data);
        new_node->next = head;
        head = new_node;
    }

    // Elimina la primera ocurrencia de un nodo con el dato especificado
    void deleteNode(std::string data) {
        if (isEmpty()) {
            return;
        }

        // Si el nodo a eliminar es la cabeza
        if (head->data == data) {
            Node* temp = head;
            head = head->next;
            delete temp; // Liberar memoria del nodo eliminado
            return;
        }

        Node* current = head;
        // Recorre hasta encontrar el nodo anterior al que se quiere eliminar
        while (current->next != nullptr && current->next->data != data) {
            current = current->next;
        }

        // Si se encontró el nodo a eliminar
        if (current->next != nullptr) {
            Node* temp = current->next;
            current->next = temp->next;
            delete temp; // Liberar memoria del nodo eliminado
        } else {
            std::cout << "Dato '" << data << "' no encontrado en la lista." << std::endl;
        }
    }

    // Muestra los elementos de la lista
    void display() const {
        if (isEmpty()) {
            std::cout << "Lista vacía." << std::endl;
            return;
        }
        Node* current = head;
        while (current != nullptr) {
            std::cout << current->data << " -> ";
            current = current->next;
        }
        std::cout << "nullptr" << std::endl;
    }
};

// Función de demostración para la Lista Enlazada
void linked_list_demo() {
    LinkedList lista;
    std::cout << "¿Lista vacía? " << (lista.isEmpty() ? "Sí" : "No") << std::endl;

    std::cout << "\n--- Añadiendo elementos (append) ---" << std::endl;
    lista.append("A");
    lista.append("B");
    lista.append("C");
    lista.display();

    std::cout << "\n--- Añadiendo al inicio (prepend) ---" << std::endl;
    lista.prepend("Inicio");
    lista.display();
    std::cout << "¿Lista vacía? " << (lista.isEmpty() ? "Sí" : "No") << std::endl;

    std::cout << "\n--- Eliminando elementos ---" << std::endl;
    lista.deleteNode("B");
    std::cout << "Después de eliminar 'B':" << std::endl;
    lista.display();

    lista.deleteNode("Inicio");
    std::cout << "Después de eliminar 'Inicio':" << std::endl;
    lista.display();

    lista.deleteNode("Z"); // Intentar eliminar un elemento que no existe
    lista.display();
}

int main() {
    linked_list_demo();
    return 0;
}

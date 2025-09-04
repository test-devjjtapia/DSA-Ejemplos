#include <iostream>
#include <queue> // Para std::queue
#include <string>

void queue_demo() {
    std::queue<std::string> cola;

    std::cout << "--- INICIALIZANDO COLA ---" << std::endl;
    std::cout << "¿La cola está vacía? " << (cola.empty() ? "Sí" : "No") << std::endl;

    std::cout << "\n--- Añadiendo elementos (enqueue) ---" << std::endl;
    cola.push("Cliente A"); // Añade al final
    cola.push("Cliente B");
    cola.push("Cliente C");
    std::cout << "Elemento en el frente (peek): " << cola.front() << std::endl;
    std::cout << "Tamaño de la cola: " << cola.size() << std::endl;

    std::cout << "\n--- Atendiendo elementos (dequeue) ---" << std::endl;
    std::string cliente_atendido = cola.front(); // Obtiene el elemento del frente
    cola.pop(); // Elimina el elemento del frente
    std::cout << "Cliente atendido: " << cliente_atendido << std::endl;

    cliente_atendido = cola.front();
    cola.pop();
    std::cout << "Cliente atendido: " << cliente_atendido << std::endl;

    std::cout << "\nPróximo cliente a atender: " << cola.front() << std::endl;
    std::cout << "¿La cola está vacía? " << (cola.empty() ? "Sí" : "No") << std::endl;

    // Intentar desencolar de una cola vacía
    std::cout << "\nIntentando desencolar de una cola vacía:" << std::endl;
    cola.pop(); // Elimina el último elemento restante
    if (cola.empty()) {
        std::cout << "La cola está vacía." << std::endl;
    }
    // Intentar acceder a front() o pop() en una cola vacía resultaría en comportamiento indefinido.
    // Se debe verificar cola.empty() antes de acceder.
}

int main() {
    queue_demo();
    return 0;
}

#include <iostream>
#include <stack> // Para std::stack
#include <string>
#include <vector> // std::vector para imprimir el contenido de la pila

// Función auxiliar para imprimir el contenido de la pila (no es un método de std::stack)
template <typename T>
void print_stack(std::stack<T> s) {
    std::vector<T> temp_vec;
    while (!s.empty()) {
        temp_vec.push_back(s.top());
        s.pop();
    }
    std::reverse(temp_vec.begin(), temp_vec.end()); // Invertir para mostrar en orden correcto
    std::cout << "[";
    for (size_t i = 0; i < temp_vec.size(); ++i) {
        std::cout << temp_vec[i];
        if (i < temp_vec.size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << "]" << std::endl;
}

void stack_demo() {
    std::stack<std::string> pila;

    std::cout << "--- INICIALIZANDO PILA ---" << std::endl;
    std::cout << "¿La pila está vacía? " << (pila.empty() ? "Sí" : "No") << std::endl;

    std::cout << "\n--- Añadiendo elementos (push) ---" << std::endl;
    pila.push("Libro 1");
    pila.push("Libro 2");
    pila.push("Libro 3");
    std::cout << "Pila actual: ";
    print_stack(pila);
    std::cout << "Elemento en la cima (peek): " << pila.top() << std::endl;
    std::cout << "Tamaño de la pila: " << pila.size() << std::endl;

    std::cout << "\n--- Eliminando elementos (pop) ---" << std::endl;
    std::string elemento_quitado = pila.top(); // Obtiene el elemento de la cima
    pila.pop(); // Elimina el elemento de la cima
    std::cout << "Elemento quitado: " << elemento_quitado << std::endl;
    std::cout << "Pila actual: ";
    print_stack(pila);

    elemento_quitado = pila.top();
    pila.pop();
    std::cout << "Elemento quitado: " << elemento_quitado << std::endl;
    std::cout << "Pila actual: ";
    print_stack(pila);

    std::cout << "\nElemento en la cima ahora: " << pila.top() << std::endl;
    std::cout << "¿La pila está vacía? " << (pila.empty() ? "Sí" : "No") << std::endl;

    // Intentar sacar de una pila vacía
    std::cout << "\nIntentando sacar de una pila vacía:" << std::endl;
    pila.pop(); // Elimina el último elemento restante
    if (pila.empty()) {
        std::cout << "Error: La pila está vacía." << std::endl;
    }
    // Intentar acceder a top() o pop() en una pila vacía resultaría en comportamiento indefinido.
    // Se debe verificar pila.empty() antes de acceder.
}

int main() {
    stack_demo();
    return 0;
}

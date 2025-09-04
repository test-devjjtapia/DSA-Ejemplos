#include <iostream>
#include <vector>
#include <string>
#include <algorithm> // Para std::remove
#include <numeric>   // Para std::iota

// Función auxiliar para imprimir vectores
template <typename T>
void print_vector(const std::vector<T>& vec) {
    std::cout << "[";
    for (size_t i = 0; i < vec.size(); ++i) {
        std::cout << vec[i];
        if (i < vec.size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << "]" << std::endl;
}

void array_demo() {
    // --- 1. Creación y Acceso ---
    std::cout << "--- Creación y Acceso ---" << std::endl;
    // En C++, std::vector es el equivalente a las listas dinámicas de Python
    std::vector<std::string> frutas = {"manzana", "banana", "cereza", "dátil"};
    std::cout << "Lista completa: ";
    print_vector(frutas);

    // Acceso por índice (basado en 0)
    std::cout << "Primer elemento (índice 0): " << frutas[0] << std::endl;
    std::cout << "Tercer elemento (índice 2): " << frutas[2] << std::endl;
    std::cout << "Último elemento (índice " << frutas.size() - 1 << "): " << frutas[frutas.size() - 1] << std::endl;

    // --- 2. Modificación ---
    std::cout << "\n--- Modificación ---" << std::endl;
    frutas[1] = "arándano";
    std::cout << "Lista después de modificar el índice 1: ";
    print_vector(frutas);

    // --- 3. Añadir elementos ---
    std::cout << "\n--- Añadir elementos ---" << std::endl;
    // Añadir al final
    frutas.push_back("frambuesa");
    std::cout << "Después de push_back('frambuesa'): ";
    print_vector(frutas);

    // Insertar en una posición específica
    frutas.insert(frutas.begin() + 2, "kiwi"); // Inserta 'kiwi' en el índice 2
    std::cout << "Después de insert(begin() + 2, 'kiwi'): ";
    print_vector(frutas);

    // --- 4. Eliminar elementos ---
    std::cout << "\n--- Eliminar elementos ---" << std::endl;
    // Eliminar el último elemento
    std::string elemento_eliminado = frutas.back();
    frutas.pop_back();
    std::cout << "Elemento eliminado con pop_back(): " << elemento_eliminado << std::endl;
    std::cout << "Lista actual: ";
    print_vector(frutas);

    // Eliminar por índice
    std::string elemento_eliminado_indice = frutas[1];
    frutas.erase(frutas.begin() + 1); // Elimina el elemento en el índice 1
    std::cout << "Elemento eliminado con erase(begin() + 1): " << elemento_eliminado_indice << std::endl;
    std::cout << "Lista actual: ";
    print_vector(frutas);

    // Eliminar por valor (la primera ocurrencia)
    // std::remove mueve el elemento al final y retorna un iterador al nuevo final
    // luego erase elimina el rango de elementos movidos.
    frutas.erase(std::remove(frutas.begin(), frutas.end(), "cereza"), frutas.end());
    std::cout << "Después de eliminar 'cereza': ";
    print_vector(frutas);

    // --- 5. Slicing (Rebanado) ---
    std::cout << "\n--- Slicing ---" << std::endl;
    std::vector<int> numeros(10);
    std::iota(numeros.begin(), numeros.end(), 0); // Rellena con 0, 1, 2, ..., 9
    std::cout << "Lista de números: ";
    print_vector(numeros);

    // Obtener un sub-arreglo (del índice 2 al 4)
    // Se crea un nuevo vector a partir de un rango de iteradores
    std::vector<int> sub_lista(numeros.begin() + 2, numeros.begin() + 5);
    std::cout << "Sub-lista de [2:5]: ";
    print_vector(sub_lista);

    // Desde el inicio hasta el índice 3
    std::vector<int> desde_inicio(numeros.begin(), numeros.begin() + 4);
    std::cout << "Sub-lista de [:4]: ";
    print_vector(desde_inicio);

    // Desde el índice 5 hasta el final
    std::vector<int> hasta_final(numeros.begin() + 5, numeros.end());
    std::cout << "Sub-lista de [5:]: ";
    print_vector(hasta_final);
}

int main() {
    array_demo();
    return 0;
}

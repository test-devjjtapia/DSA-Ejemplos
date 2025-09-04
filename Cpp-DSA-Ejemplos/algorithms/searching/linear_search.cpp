#include <iostream>
#include <vector>
#include <string>
#include <numeric> // Para std::iota

// Realiza una búsqueda lineal en un vector de enteros.
// Retorna el índice del elemento si se encuentra, de lo contrario -1.
int linearSearch(const std::vector<int>& arr, int target) {
    for (int i = 0; i < arr.size(); ++i) {
        if (arr[i] == target) {
            return i; // Se encontró el elemento, se devuelve el índice
        }
    }
    return -1; // El elemento no fue encontrado
}

void linear_search_demo() {
    std::vector<int> mi_lista = {10, 50, 30, 70, 80, 20, 90, 40};

    std::cout << "Lista: ";
    for (int val : mi_lista) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    // --- Búsqueda de un elemento que existe ---
    int objetivo1 = 80;
    int indice1 = linearSearch(mi_lista, objetivo1);
    if (indice1 != -1) {
        std::cout << "El elemento " << objetivo1 << " se encuentra en el índice: " << indice1 << std::endl;
    } else {
        std::cout << "El elemento " << objetivo1 << " no se encontró en la lista." << std::endl;
    }

    // --- Búsqueda de un elemento que NO existe ---
    int objetivo2 = 100;
    int indice2 = linearSearch(mi_lista, objetivo2);
    if (indice2 != -1) {
        std::cout << "El elemento " << objetivo2 << " se encuentra en el índice: " << indice2 << std::endl;
    } else {
        std::cout << "El elemento " << objetivo2 << " no se encontró en la lista." << std::endl;
    }
}

int main() {
    linear_search_demo();
    return 0;
}

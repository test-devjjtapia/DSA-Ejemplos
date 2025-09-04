#include <iostream>
#include <vector>
#include <algorithm> // Para std::sort si fuera necesario, pero la lista ya está ordenada

// Realiza una búsqueda binaria en un vector ordenado de enteros.
// Retorna el índice del elemento si se encuentra, de lo contrario -1.
int binarySearch(const std::vector<int>& arr, int target) {
    int low = 0;
    int high = arr.size() - 1;

    while (low <= high) {
        int mid = low + (high - low) / 2; // Evita desbordamiento potencial
        int guess = arr[mid];

        if (guess == target) {
            return mid; // Elemento encontrado
        } else if (guess > target) {
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }
    return -1; // Elemento no encontrado
}

void binary_search_demo() {
    // La lista DEBE estar ordenada
    std::vector<int> mi_lista_ordenada = {2, 5, 8, 12, 16, 23, 38, 56, 72, 91};

    std::cout << "Lista ordenada: ";
    for (int val : mi_lista_ordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    // --- Búsqueda de un elemento que existe ---
    int objetivo1 = 23;
    int indice1 = binarySearch(mi_lista_ordenada, objetivo1);
    if (indice1 != -1) {
        std::cout << "El elemento " << objetivo1 << " se encuentra en el índice: " << indice1 << std::endl;
    } else {
        std::cout << "El elemento " << objetivo1 << " no se encontró en la lista." << std::endl;
    }

    // --- Búsqueda de un elemento que NO existe ---
    int objetivo2 = 40;
    int indice2 = binarySearch(mi_lista_ordenada, objetivo2);
    if (indice2 != -1) {
        std::cout << "El elemento " << objetivo2 << " se encuentra en el índice: " << indice2 << std::endl;
    } else {
        std::cout << "El elemento " << objetivo2 << " no se encontró en la lista." << std::endl;
    }
}

int main() {
    binary_search_demo();
    return 0;
}

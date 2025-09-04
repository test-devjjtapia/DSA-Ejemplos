#include <iostream>
#include <vector>
#include <algorithm> // Para std::swap

// Ordena un vector de enteros utilizando el método de la burbuja.
void bubbleSort(std::vector<int>& arr) {
    int n = arr.size();
    // Recorremos todos los elementos del arreglo
    for (int i = 0; i < n - 1; ++i) {
        // Los últimos i elementos ya están en su lugar
        for (int j = 0; j < n - i - 1; ++j) {
            // Comparamos elementos adyacentes y los intercambiamos si están en el orden incorrecto
            if (arr[j] > arr[j + 1]) {
                std::swap(arr[j], arr[j + 1]);
            }
        }
    }
}

void bubble_sort_demo() {
    std::vector<int> mi_lista_desordenada = {64, 34, 25, 12, 22, 11, 90};

    std::cout << "Lista desordenada: ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    bubbleSort(mi_lista_desordenada);

    std::cout << "Lista ordenada:    ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    std::vector<int> otra_lista = {5, 1, 4, 2, 8};
    std::cout << "\nLista desordenada: ";
    for (int val : otra_lista) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
    bubbleSort(otra_lista);
    std::cout << "Lista ordenada:    ";
    for (int val : otra_lista) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
}

int main() {
    bubble_sort_demo();
    return 0;
}

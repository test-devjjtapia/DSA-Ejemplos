#include <iostream>
#include <vector>
#include <algorithm> // Para std::swap

// Ordena un vector de enteros utilizando el método de selección.
void selectionSort(std::vector<int>& arr) {
    int n = arr.size();

    // Recorrer toda la lista
    for (int i = 0; i < n - 1; ++i) {
        // Encontrar el mínimo en la sublista no ordenada
        int min_idx = i;
        for (int j = i + 1; j < n; ++j) {
            if (arr[j] < arr[min_idx]) {
                min_idx = j;
            }
        }
        // Intercambiar el elemento mínimo encontrado con el primer elemento de la sublista no ordenada
        std::swap(arr[i], arr[min_idx]);
    }
}

void selection_sort_demo() {
    std::vector<int> mi_lista_desordenada = {64, 25, 12, 22, 11};

    std::cout << "Lista desordenada: ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    selectionSort(mi_lista_desordenada);

    std::cout << "Lista ordenada:    ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    std::vector<int> otra_lista = {38, 27, 43, 3, 9, 82, 10};
    std::cout << "\nLista desordenada: ";
    for (int val : otra_lista) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
    selectionSort(otra_lista);
    std::cout << "Lista ordenada:    ";
    for (int val : otra_lista) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
}

int main() {
    selection_sort_demo();
    return 0;
}

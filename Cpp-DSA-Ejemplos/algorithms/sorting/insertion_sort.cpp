#include <iostream>
#include <vector>

// Ordena un vector de enteros utilizando el método de inserción.
void insertionSort(std::vector<int>& arr) {
    int n = arr.size();
    // Recorremos desde el segundo elemento
    for (int i = 1; i < n; ++i) {
        int key = arr[i]; // El elemento que vamos a insertar
        int j = i - 1;

        // Mover los elementos de arr[0..i-1] que son mayores que key
        // a una posición adelante de su posición actual
        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        arr[j + 1] = key;
    }
}

void insertion_sort_demo() {
    std::vector<int> mi_lista_desordenada = {12, 11, 13, 5, 6};

    std::cout << "Lista desordenada: ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    insertionSort(mi_lista_desordenada);

    std::cout << "Lista ordenada:    ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    std::vector<int> otra_lista = {64, 34, 25, 12, 22, 11, 90};
    std::cout << "\nLista desordenada: ";
    for (int val : otra_lista) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
    insertionSort(otra_lista);
    std::cout << "Lista ordenada:    ";
    for (int val : otra_lista) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
}

int main() {
    insertion_sort_demo();
    return 0;
}

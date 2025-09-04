#include <iostream>
#include <vector>
#include <algorithm> // Para std::swap

// Función auxiliar para particionar el vector
int partition(std::vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Tomamos el último elemento como pivote
    int i = (low - 1);     // Índice del elemento más pequeño

    for (int j = low; j < high; ++j) {
        // Si el elemento actual es menor o igual que el pivote
        if (arr[j] <= pivot) {
            i++;
            std::swap(arr[i], arr[j]);
        }
    }
    std::swap(arr[i + 1], arr[high]); // Coloca el pivote en su posición correcta
    return (i + 1);
}

// Ordena un vector de enteros utilizando el método Quick Sort.
void quickSort(std::vector<int>& arr, int low, int high) {
    if (low < high) {
        // pi es el índice de partición, arr[pi] está ahora en su lugar correcto
        int pi = partition(arr, low, high);

        // Ordena recursivamente los elementos antes y después de la partición
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}

void quick_sort_demo() {
    std::vector<int> mi_lista_desordenada = {10, 7, 8, 9, 1, 5};

    std::cout << "Lista desordenada: ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    quickSort(mi_lista_desordenada, 0, mi_lista_desordenada.size() - 1);

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
    quickSort(otra_lista, 0, otra_lista.size() - 1);
    std::cout << "Lista ordenada:    ";
    for (int val : otra_lista) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
}

int main() {
    quick_sort_demo();
    return 0;
}

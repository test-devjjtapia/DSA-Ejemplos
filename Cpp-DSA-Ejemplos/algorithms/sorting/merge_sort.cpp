#include <iostream>
#include <vector>
#include <algorithm> // Para std::copy

// Función auxiliar para fusionar dos sub-vectores ordenados.
void merge(std::vector<int>& arr, int left, int mid, int right) {
    int n1 = mid - left + 1;
    int n2 = right - mid;

    // Crear vectores temporales
    std::vector<int> L(n1);
    std::vector<int> R(n2);

    // Copiar datos a los vectores temporales L[] y R[]
    for (int i = 0; i < n1; ++i) {
        L[i] = arr[left + i];
    }
    for (int j = 0; j < n2; ++j) {
        R[j] = arr[mid + 1 + j];
    }

    // Fusionar los vectores temporales de vuelta en arr[left..right]
    int i = 0; // Índice inicial del primer sub-vector
    int j = 0; // Índice inicial del segundo sub-vector
    int k = left; // Índice inicial del sub-vector fusionado

    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        } else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    // Copiar los elementos restantes de L[], si los hay
    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    // Copiar los elementos restantes de R[], si los hay
    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

// Ordena un vector de enteros utilizando el método Merge Sort.
void mergeSort(std::vector<int>& arr, int left, int right) {
    if (left >= right) {
        return; // Caso base: el sub-vector tiene 0 o 1 elemento
    }

    int mid = left + (right - left) / 2; // Encontrar el medio del sub-vector

    // Ordenar la primera mitad recursivamente
    mergeSort(arr, left, mid);
    // Ordenar la segunda mitad recursivamente
    mergeSort(arr, mid + 1, right);

    // Fusionar las dos mitades ordenadas
    merge(arr, left, mid, right);
}

void merge_sort_demo() {
    std::vector<int> mi_lista_desordenada = {38, 27, 43, 3, 9, 82, 10};

    std::cout << "Lista desordenada: ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    mergeSort(mi_lista_desordenada, 0, mi_lista_desordenada.size() - 1);

    std::cout << "Lista ordenada:    ";
    for (int val : mi_lista_desordenada) {
        std::cout << val << " ";
    }
    std::cout << std::endl;
}

int main() {
    merge_sort_demo();
    return 0;
}

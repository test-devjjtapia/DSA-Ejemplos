#include <iostream>
#include <vector>
#include <map>
#include <chrono> // Para medir el tiempo

// --- 1. Enfoque Recursivo (Ingenuo) ---
// Calcula el n-ésimo número de Fibonacci usando recursión simple.
long long fibRecursive(int n) {
    if (n <= 1) {
        return n;
    }
    return fibRecursive(n - 1) + fibRecursive(n - 2);
}

// --- 2. Enfoque Iterativo (Eficiente) ---
// Calcula el n-ésimo número de Fibonacci usando un bucle.
long long fibIterative(int n) {
    if (n <= 1) {
        return n;
    }
    long long a = 0;
    long long b = 1;
    for (int i = 2; i <= n; ++i) {
        long long temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}

// --- 3. Enfoque Recursivo con Memoización (Optimizado) ---
// Mapa para almacenar los resultados ya calculados (memoización)
std::map<int, long long> memo;

// Calcula el n-ésimo número de Fibonacci usando recursión con memoización.
long long fibMemoized(int n) {
    if (n <= 1) {
        return n;
    }
    // Si el resultado ya está en el mapa, retornarlo
    if (memo.count(n)) {
        return memo[n];
    }

    // Calcular el resultado y almacenarlo en el mapa antes de retornarlo
    long long result = fibMemoized(n - 1) + fibMemoized(n - 2);
    memo[n] = result;
    return result;
}

void fibonacci_demo() {
    int num = 35; // Para números grandes, la recursiva ingenua será muy lenta

    std::cout << "Calculando el Fibonacci de " << num << " con diferentes métodos:\n" << std::endl;

    // Iterativo (el más rápido)
    auto start_time = std::chrono::high_resolution_clock::now();
    long long result_iter = fibIterative(num);
    auto end_time = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> duration_iter = end_time - start_time;
    std::cout << "Iterativo:     " << result_iter << " (Tiempo: " << duration_iter.count() << " segundos)" << std::endl;

    // Memoizado
    memo.clear(); // Limpiar memoización para cada ejecución
    start_time = std::chrono::high_resolution_clock::now();
    long long result_memo = fibMemoized(num);
    end_time = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> duration_memo = end_time - start_time;
    std::cout << "Memoizado:     " << result_memo << " (Tiempo: " << duration_memo.count() << " segundos)" << std::endl;

    // Recursivo (el más lento, puede tardar mucho)
    std::cout << "\nEl cálculo recursivo puro puede ser muy lento, ten paciencia..." << std::endl;
    start_time = std::chrono::high_resolution_clock::now();
    long long result_rec = fibRecursive(num);
    end_time = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> duration_rec = end_time - start_time;
    std::cout << "Recursivo puro: " << result_rec << " (Tiempo: " << duration_rec.count() << " segundos)" << std::endl;
}

int main() {
    fibonacci_demo();
    return 0;
}

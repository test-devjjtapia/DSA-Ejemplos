package algorithms.recursion;

import java.util.HashMap;
import java.util.Map;

public class Fibonacci {

    // --- 1. Enfoque Recursivo (Ingenuo) ---
    public static long fibRecursive(int n) {
        if (n <= 1) {
            return n;
        }
        return fibRecursive(n - 1) + fibRecursive(n - 2);
    }

    // --- 2. Enfoque Iterativo (Eficiente) ---
    public static long fibIterative(int n) {
        if (n <= 1) {
            return n;
        }
        long a = 0;
        long b = 1;
        for (int i = 2; i <= n; i++) {
            long temp = a + b;
            a = b;
            b = temp;
        }
        return b;
    }

    // --- 3. Enfoque Recursivo con Memoización (Optimizado) ---
    private static Map<Integer, Long> memo = new HashMap<>();

    public static long fibMemoized(int n) {
        if (n <= 1) {
            return n;
        }
        if (memo.containsKey(n)) {
            return memo.get(n);
        }

        long result = fibMemoized(n - 1) + fibMemoized(n - 2);
        memo.put(n, result);
        return result;
    }

    public static void main(String[] args) {
        int num = 35; // Para números grandes, la recursiva ingenua será muy lenta

        System.out.println("Calculando el Fibonacci de " + num + " con diferentes métodos:\n");

        // Iterativo (el más rápido)
        long startTime = System.nanoTime();
        long resultIter = fibIterative(num);
        long endTime = System.nanoTime();
        System.out.printf("Iterativo:     %d (Tiempo: %.6f segundos)\n", resultIter, (endTime - startTime) / 1_000_000_000.0);

        // Memoizado
        memo.clear(); // Limpiar memoización para cada ejecución
        startTime = System.nanoTime();
        long resultMemo = fibMemoized(num);
        endTime = System.nanoTime();
        System.out.printf("Memoizado:     %d (Tiempo: %.6f segundos)\n", resultMemo, (endTime - startTime) / 1_000_000_000.0);

        // Recursivo (el más lento, puede tardar mucho)
        System.out.println("\nEl cálculo recursivo puro puede ser muy lento, ten paciencia...");
        startTime = System.nanoTime();
        long resultRec = fibRecursive(num);
        endTime = System.nanoTime();
        System.out.printf("Recursivo puro: %d (Tiempo: %.6f segundos)\n", resultRec, (endTime - startTime) / 1_000_000_000.0);
    }
}

//! Módulo para demostrar el cálculo de la secuencia de Fibonacci en Rust.

use std::collections::HashMap;
use std::time::Instant;

// --- 1. Enfoque Recursivo (Ingenuo) ---
// Calcula el n-ésimo número de Fibonacci usando recursión simple.
pub fn fib_recursive(n: u64) -> u64 {
    if n <= 1 {
        return n;
    }
    fib_recursive(n - 1) + fib_recursive(n - 2)
}

// --- 2. Enfoque Iterativo (Eficiente) ---
// Calcula el n-ésimo número de Fibonacci usando un bucle.
pub fn fib_iterative(n: u64) -> u64 {
    if n <= 1 {
        return n;
    }
    let mut a = 0;
    let mut b = 1;
    for _ in 2..=n {
        let next = a + b;
        a = b;
        b = next;
    }
    b
}

// --- 3. Enfoque Recursivo con Memoización (Optimizado) ---
// Calcula el n-ésimo número de Fibonacci usando recursión con memoización.
pub fn fib_memoized(n: u64, memo: &mut HashMap<u64, u64>) -> u64 {
    if n <= 1 {
        return n;
    }
    if let Some(&val) = memo.get(&n) {
        return val;
    }

    let result = fib_memoized(n - 1, memo) + fib_memoized(n - 2, memo);
    memo.insert(n, result);
    result
}

// Función de demostración para Fibonacci
pub fn fibonacci_demo() {
    let num = 35; // Para números grandes, la recursiva ingenua será muy lenta

    println!("Calculando el Fibonacci de {} con diferentes métodos:\n", num);

    // Iterativo (el más rápido)
    let start_time = Instant::now();
    let result_iter = fib_iterative(num);
    let end_time = Instant::now();
    println!("Iterativo:     {} (Tiempo: {:.6?} segundos)", result_iter, end_time.duration_since(start_time));

    // Memoizado
    let mut memo = HashMap::new(); // Limpiar memoización para cada ejecución
    let start_time = Instant::now();
    let result_memo = fib_memoized(num, &mut memo);
    let end_time = Instant::now();
    println!("Memoizado:     {} (Tiempo: {:.6?} segundos)", result_memo, end_time.duration_since(start_time));

    // Recursivo (el más lento, puede tardar mucho)
    println!("\nEl cálculo recursivo puro puede ser muy lento, ten paciencia...");
    let start_time = Instant::now();
    let result_rec = fib_recursive(num);
    let end_time = Instant::now();
    println!("Recursivo puro: {} (Tiempo: {:.6?} segundos)", result_rec, end_time.duration_since(start_time));
}

"""
Ejemplos de cálculo de la secuencia de Fibonacci.

La secuencia de Fibonacci es una serie de números donde cada número es la suma
de los dos anteriores, comenzando usualmente con 0 y 1.
Secuencia: 0, 1, 1, 2, 3, 5, 8, 13, 21, ...

Se utiliza a menudo para ilustrar la recursión y la optimización (memoización).
"""
import time

# --- 1. Enfoque Recursivo (Ingenuo) ---
def fib_recursive(n):
    """Calcula el n-ésimo número de Fibonacci usando recursión simple."""
    if n <= 1:
        return n
    return fib_recursive(n-1) + fib_recursive(n-2)

# --- 2. Enfoque Iterativo (Eficiente) ---
def fib_iterative(n):
    """Calcula el n-ésimo número de Fibonacci usando un bucle."""
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(n - 1):
        a, b = b, a + b
    return b

# --- 3. Enfoque Recursivo con Memoización (Optimizado) ---
memo = {}
def fib_memoized(n):
    """Calcula el n-ésimo número de Fibonacci usando recursión con memoización."""
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    
    result = fib_memoized(n-1) + fib_memoized(n-2)
    memo[n] = result
    return result

# --- Ejemplo de Uso y Comparación ---
if __name__ == "__main__":
    num = 35

    print(f"Calculando el Fibonacci de {num} con diferentes métodos:\n")

    # Iterativo (el más rápido)
    start_time = time.time()
    result_iter = fib_iterative(num)
    end_time = time.time()
    print(f"Iterativo:     {result_iter} (Tiempo: {end_time - start_time:.6f} segundos)")

    # Memoizado
    start_time = time.time() 
    result_memo = fib_memoized(num)
    end_time = time.time()
    print(f"Memoizado:     {result_memo} (Tiempo: {end_time - start_time:.6f} segundos)")

    # Recursivo (el más lento, puede tardar mucho)
    print("\nEl cálculo recursivo puro puede ser muy lento, ten paciencia...")
    start_time = time.time()
    result_rec = fib_recursive(num)
    end_time = time.time()
    print(f"Recursivo puro: {result_rec} (Tiempo: {end_time - start_time:.6f} segundos)")

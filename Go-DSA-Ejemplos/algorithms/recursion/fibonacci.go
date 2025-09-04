package recursion

import (
	"fmt"
	"time"
)

// --- 1. Enfoque Recursivo (Ingenuo) ---
func FibRecursive(n int) int {
	if n <= 1 {
		return n
	}
	return FibRecursive(n-1) + FibRecursive(n-2)
}

// --- 2. Enfoque Iterativo (Eficiente) ---
func FibIterative(n int) int {
	if n <= 1 {
		return n
	}
	a, b := 0, 1
	for i := 2; i <= n; i++ {
		next := a + b
		a = b
		b = next
	}
	return b
}

// --- 3. Enfoque Recursivo con Memoización (Optimizado) ---
var memo = make(map[int]int)

func FibMemoized(n int) int {
	if n <= 1 {
		return n
	}
	if val, ok := memo[n]; ok {
		return val
	}

	result := FibMemoized(n-1) + FibMemoized(n-2)
	memo[n] = result
	return result
}

func main() {
	num := 35 // Para números grandes, la recursiva ingenua será muy lenta

	fmt.Printf("Calculando el Fibonacci de %d con diferentes métodos:\n\n", num)

	// Iterativo (el más rápido)
	startTime := time.Now()
	resultIter := FibIterative(num)
	endTime := time.Now()
	fmt.Printf("Iterativo:     %d (Tiempo: %.6f segundos)\n", resultIter, endTime.Sub(startTime).Seconds())

	// Memoizado
	memo = make(map[int]int) // Limpiar memoización para cada ejecución
	startTime = time.Now()
	resultMemo := FibMemoized(num)
	endTime = time.Now()
	fmt.Printf("Memoizado:     %d (Tiempo: %.6f segundos)\n", resultMemo, endTime.Sub(startTime).Seconds())

	// Recursivo (el más lento, puede tardar mucho)
	fmt.Println("\nEl cálculo recursivo puro puede ser muy lento, ten paciencia...")
	startTime = time.Now()
	resultRec := FibRecursive(num)
	endTime = time.Now()
	fmt.Printf("Recursivo puro: %d (Tiempo: %.6f segundos)\n", resultRec, endTime.Sub(startTime).Seconds())
}

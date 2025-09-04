package data_structures

import (
	"fmt"
	"strconv"
)

func ArrayDemo() {
	// --- 1. Creación y Acceso ---
	fmt.Println("--- Creación y Acceso ---")
	// En Go, los slices son el equivalente a las listas dinámicas de Python
	frutas := []string{"manzana", "banana", "cereza", "dátil"}
	fmt.Println("Lista completa:", frutas)

	// Acceso por índice (basado en 0)
	fmt.Println("Primer elemento (índice 0):", frutas[0])
	fmt.Println("Tercer elemento (índice 2):", frutas[2])
	fmt.Println("Último elemento (índice", len(frutas)-1, "):", frutas[len(frutas)-1])

	// --- 2. Modificación ---
	fmt.Println("\n--- Modificación ---")
	frutas[1] = "arándano"
	fmt.Println("Lista después de modificar el índice 1:", frutas)

	// --- 3. Añadir elementos ---
	fmt.Println("\n--- Añadir elementos ---")
	// Añadir al final
	frutas = append(frutas, "frambuesa")
	fmt.Println("Después de append('frambuesa'):", frutas)

	// Insertar en una posición específica
	// Para insertar, necesitamos crear un nuevo slice y copiar los elementos
	frutas = append(frutas[:2], append([]string{"kiwi"}, frutas[2:]...)...)
	fmt.Println("Después de insert(2, 'kiwi'):", frutas)

	// --- 4. Eliminar elementos ---
	fmt.Println("\n--- Eliminar elementos ---")
	// Eliminar el último elemento
	elementoEliminado := frutas[len(frutas)-1]
	frutas = frutas[:len(frutas)-1]
	fmt.Println("Elemento eliminado con pop():", elementoEliminado)
	fmt.Println("Lista actual:", frutas)

	// Eliminar por índice
	elementoEliminadoIndice := frutas[1]
	frutas = append(frutas[:1], frutas[2:]...)
	fmt.Println("Elemento eliminado con remove(1):", elementoEliminadoIndice)
	fmt.Println("Lista actual:", frutas)

	// Eliminar por valor (la primera ocurrencia)
	for i, v := range frutas {
		if v == "cereza" {
			frutas = append(frutas[:i], frutas[i+1:]...)
			break
		}
	}
	fmt.Println("Después de remove('cereza'):", frutas)

	// --- 5. Slicing (Rebanado) ---
	fmt.Println("\n--- Slicing ---")
	numeros := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
	fmt.Println("Lista de números:", numeros)

	// Obtener un sub-arreglo (del índice 2 al 4)
	subLista := numeros[2:5]
	fmt.Println("Sub-lista de [2:5]:", subLista)

	// Desde el inicio hasta el índice 3
	desdeInicio := numeros[:4]
	fmt.Println("Sub-lista de [:4]:", desdeInicio)

	// Desde el índice 5 hasta el final
	hastaFinal := numeros[5:]
	fmt.Println("Sub-lista de [5:]:", hastaFinal)
}

func main() {
	ArrayDemo()
}

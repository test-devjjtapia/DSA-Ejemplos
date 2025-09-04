package searching

import "fmt"

func LinearSearch(arr []int, target int) int {
	for i, v := range arr {
		if v == target {
			return i // Element found, return its index
		}
	}
	return -1 // Element not found
}

func main() {
	myList := []int{10, 50, 30, 70, 80, 20, 90, 40}

	fmt.Println("Lista:", myList)

	// --- Búsqueda de un elemento que existe ---
	target1 := 80
	index1 := LinearSearch(myList, target1)
	if index1 != -1 {
		fmt.Printf("El elemento %d se encuentra en el índice: %d\n", target1, index1)
	} else {
		fmt.Printf("El elemento %d no se encontró en la lista.\n", target1)
	}

	// --- Búsqueda de un elemento que NO existe ---
	target2 := 100
	index2 := LinearSearch(myList, target2)
	if index2 != -1 {
		fmt.Printf("El elemento %d se encuentra en el índice: %d\n", target2, index2)
	} else {
		fmt.Printf("El elemento %d no se encontró en la lista.\n", target2)
	}
}

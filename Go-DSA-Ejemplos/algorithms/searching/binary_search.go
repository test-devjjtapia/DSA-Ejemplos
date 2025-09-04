package searching

import "fmt"

func BinarySearch(arr []int, target int) int {
	low := 0
	high := len(arr) - 1

	for low <= high {
		mid := low + (high-low)/2
		guess := arr[mid]

		if guess == target {
			return mid // Element found
		} else if guess > target {
			high = mid - 1
		} else {
			low = mid + 1
		}
	}
	return -1 // Element not found
}

func main() {
	// The list MUST be sorted
	sortedList := []int{2, 5, 8, 12, 16, 23, 38, 56, 72, 91}

	fmt.Println("Lista ordenada:", sortedList)

	// --- Búsqueda de un elemento que existe ---
	target1 := 23
	index1 := BinarySearch(sortedList, target1)
	if index1 != -1 {
		fmt.Printf("El elemento %d se encuentra en el índice: %d\n", target1, index1)
	} else {
		fmt.Printf("El elemento %d no se encontró en la lista.\n", target1)
	}

	// --- Búsqueda de un elemento que NO existe ---
	target2 := 40
	index2 := BinarySearch(sortedList, target2)
	if index2 != -1 {
		fmt.Printf("El elemento %d se encuentra en el índice: %d\n", target2, index2)
	} else {
		fmt.Printf("El elemento %d no se encontró en la lista.\n", target2)
	}
}

package sorting

import "fmt"

func SelectionSort(arr []int) {
	n := len(arr)
	for i := 0; i < n-1; i++ {
		minIdx := i
		for j := i + 1; j < n; j++ {
			if arr[j] < arr[minIdx] {
				minIdx = j
			}
		}
		arr[i], arr[minIdx] = arr[minIdx], arr[i] // Swap
	}
}

func main() {
	myList := []int{64, 25, 12, 22, 11}

	fmt.Println("Lista desordenada:", myList)

	SelectionSort(myList)

	fmt.Println("Lista ordenada:   ", myList)

	anotherList := []int{38, 27, 43, 3, 9, 82, 10}
	fmt.Println("\nLista desordenada:", anotherList)
	SelectionSort(anotherList)
	fmt.Println("Lista ordenada:   ", anotherList)
}

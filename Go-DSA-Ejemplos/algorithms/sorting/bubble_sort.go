package sorting

import "fmt"

func BubbleSort(arr []int) {
	n := len(arr)
	for i := 0; i < n-1; i++ {
		for j := 0; j < n-i-1; j++ {
			if arr[j] > arr[j+1] {
				arr[j], arr[j+1] = arr[j+1], arr[j] // Swap
			}
		}
	}
}

func main() {
	myList := []int{64, 34, 25, 12, 22, 11, 90}

	fmt.Println("Lista desordenada:", myList)

	BubbleSort(myList)

	fmt.Println("Lista ordenada:   ", myList)

	anotherList := []int{5, 1, 4, 2, 8}
	fmt.Println("\nLista desordenada:", anotherList)
	BubbleSort(anotherList)
	fmt.Println("Lista ordenada:   ", anotherList)
}

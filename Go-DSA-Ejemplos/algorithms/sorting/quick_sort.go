package sorting

import "fmt"

func QuickSort(arr []int, low, high int) {
	if low < high {
		pi := partition(arr, low, high)

		QuickSort(arr, low, pi-1)
		QuickSort(arr, pi+1, high)
	}
}

func partition(arr []int, low, high int) int {
	pivot := arr[high]
	i := (low - 1)

	for j := low; j < high; j++ {
		if arr[j] <= pivot {
			i++
			arr[i], arr[j] = arr[j], arr[i] // Swap
		}
	}
	arr[i+1], arr[high] = arr[high], arr[i+1] // Swap pivot
	return i + 1
}

func main() {
	myList := []int{10, 7, 8, 9, 1, 5}

	fmt.Println("Lista desordenada:", myList)

	QuickSort(myList, 0, len(myList)-1)

	fmt.Println("Lista ordenada:   ", myList)

	anotherList := []int{64, 34, 25, 12, 22, 11, 90}
	fmt.Println("\nLista desordenada:", anotherList)
	QuickSort(anotherList, 0, len(anotherList)-1)
	fmt.Println("Lista ordenada:   ", anotherList)
}

package sorting

import "fmt"

func InsertionSort(arr []int) {
	n := len(arr)
	for i := 1; i < n; i++ {
		key := arr[i]
		j := i - 1

		for j >= 0 && arr[j] > key {
			arr[j+1] = arr[j]
			j = j - 1
		}
		arr[j+1] = key
	}
}

func main() {
	myList := []int{12, 11, 13, 5, 6}

	fmt.Println("Lista desordenada:", myList)

	InsertionSort(myList)

	fmt.Println("Lista ordenada:   ", myList)

	anotherList := []int{64, 34, 25, 12, 22, 11, 90}
	fmt.Println("\nLista desordenada:", anotherList)
	InsertionSort(anotherList)
	fmt.Println("Lista ordenada:   ", anotherList)
}

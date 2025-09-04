package recursion

import "fmt"

func ReverseListRecursive(list []string) []string {
	// Base case: if the list is empty or has one element, it's already reversed.
	if len(list) <= 1 {
		return list
	}

	// Recursive step:
	// 1. Take the first element (list[0])
	// 2. Recursively call the function with the rest of the list (list[1:])
	// 3. Concatenate the first element to the end of the returned reversed list.
	reversedTail := ReverseListRecursive(list[1:])
	return append(reversedTail, list[0])
}

func main() {
	myList := []string{"1", "2", "3", "4", "5"}
	fmt.Println("Lista original:", myList)

	listaInvertida := ReverseListRecursive(myList)
	fmt.Println("Lista invertida:", listaInvertida)

	// Example with another list
	anotherList := []string{"a", "b", "c", "d"}
	fmt.Println("\nLista original:", anotherList)
	listaInvertida2 := ReverseListRecursive(anotherList)
	fmt.Println("Lista invertida:", listaInvertida2)
}

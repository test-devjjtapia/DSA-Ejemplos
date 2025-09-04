package data_structures

import "fmt"

type Node struct {
	data string
	next *Node
}

type LinkedList struct {
	head *Node
}

func (ll *LinkedList) IsEmpty() bool {
	return ll.head == nil
}

func (ll *LinkedList) Append(data string) {
	newNode := &Node{data: data, next: nil}
	if ll.IsEmpty() {
		ll.head = newNode
		return
	}

	lastNode := ll.head
	for lastNode.next != nil {
		lastNode = lastNode.next
	}
	lastNode.next = newNode
}

func (ll *LinkedList) Prepend(data string) {
	newNode := &Node{data: data, next: ll.head}
	ll.head = newNode
}

func (ll *LinkedList) Delete(data string) {
	if ll.IsEmpty() {
		return
	}

	// If the node to delete is the head
	if ll.head.data == data {
		ll.head = ll.head.next
		return
	}

	currentNode := ll.head
	for currentNode.next != nil && currentNode.next.data != data {
		currentNode = currentNode.next
	}

	// If the node to delete is found
	if currentNode.next != nil {
		currentNode.next = currentNode.next.next
	} else {
		fmt.Printf("Dato '%s' no encontrado en la lista.\n", data)
	}
}

func (ll *LinkedList) Display() {
	if ll.IsEmpty() {
		fmt.Println("Lista vacía.")
		return
	}
	currentNode := ll.head
	for currentNode != nil {
		fmt.Printf("%s -> ", currentNode.data)
		currentNode = currentNode.next
	}
	fmt.Println("None")
}

func LinkedListDemo() {
	lista := LinkedList{}
	fmt.Println("¿Lista vacía?", lista.IsEmpty())

	fmt.Println("\n--- Añadiendo elementos (Append) ---")
	lista.Append("A")
	lista.Append("B")
	lista.Append("C")
	lista.Display()

	fmt.Println("\n--- Añadiendo al inicio (Prepend) ---")
	lista.Prepend("Inicio")
	lista.Display()
	fmt.Println("¿Lista vacía?", lista.IsEmpty())

	fmt.Println("\n--- Eliminando elementos ---")
	lista.Delete("B")
	fmt.Println("Después de eliminar 'B':")
	lista.Display()

	lista.Delete("Inicio")
	fmt.Println("Después de eliminar 'Inicio':")
	lista.Display()

	lista.Delete("Z") // Intentar eliminar un elemento que no existe
	lista.Display()
}

func main() {
	LinkedListDemo()
}

package data_structures

import "fmt"

type Stack struct {
	items []string
}

func (s *Stack) IsEmpty() bool {
	return len(s.items) == 0
}

func (s *Stack) Push(item string) {
	s.items = append(s.items, item)
}

func (s *Stack) Pop() (string, bool) {
	if s.IsEmpty() {
		return "", false // Indicate error or empty stack
	}
	lastIndex := len(s.items) - 1
	item := s.items[lastIndex]
	s.items = s.items[:lastIndex]
	return item, true
}

func (s *Stack) Peek() (string, bool) {
	if s.IsEmpty() {
		return "", false
	}
	return s.items[len(s.items)-1], true
}

func (s *Stack) Size() int {
	return len(s.items)
}

func StackDemo() {
	pila := Stack{}

	fmt.Println("--- INICIALIZANDO PILA ---")
	fmt.Println("¿La pila está vacía?", pila.IsEmpty())

	fmt.Println("\n--- Añadiendo elementos (Push) ---")
	pila.Push("Libro 1")
	pila.Push("Libro 2")
	pila.Push("Libro 3")
	fmt.Println("Pila actual:", pila.items)
	peeked, ok := pila.Peek()
	if ok {
		fmt.Println("Elemento en la cima (Peek):", peeked)
	}
	fmt.Println("Tamaño de la pila:", pila.Size())

	fmt.Println("\n--- Eliminando elementos (Pop) ---")
	elementoQuitado, ok := pila.Pop()
	if ok {
		fmt.Println("Elemento quitado:", elementoQuitado)
	}
	fmt.Println("Pila actual:", pila.items)

	elementoQuitado, ok = pila.Pop()
	if ok {
		fmt.Println("Elemento quitado:", elementoQuitado)
	}
	fmt.Println("Pila actual:", pila.items)

	peeked, ok = pila.Peek()
	if ok {
		fmt.Println("Elemento en la cima ahora:", peeked)
	} else {
		fmt.Println("La pila está vacía, no hay elemento en la cima.")
	}
	fmt.Println("¿La pila está vacía?", pila.IsEmpty())

	// Intentar sacar de una pila vacía
	fmt.Println("\nIntentando sacar de una pila vacía:")
	_, ok = pila.Pop()
	if !ok {
		fmt.Println("Error: La pila está vacía.")
	}
}

func main() {
	StackDemo()
}

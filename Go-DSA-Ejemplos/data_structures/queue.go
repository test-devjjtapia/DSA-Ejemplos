package data_structures

import "fmt"

type Queue struct {
	items []string
}

func (q *Queue) IsEmpty() bool {
	return len(q.items) == 0
}

func (q *Queue) Enqueue(item string) {
	q.items = append(q.items, item)
}

func (q *Queue) Dequeue() (string, bool) {
	if q.IsEmpty() {
		return "", false // Indicate error or empty queue
	}
	item := q.items[0]
	q.items = q.items[1:]
	return item, true
}

func (q *Queue) Peek() (string, bool) {
	if q.IsEmpty() {
		return "", false
	}
	return q.items[0], true
}

func (q *Queue) Size() int {
	return len(q.items)
}

func QueueDemo() {
	cola := Queue{}

	fmt.Println("--- INICIALIZANDO COLA ---")
	fmt.Println("¿La cola está vacía?", cola.IsEmpty())

	fmt.Println("\n--- Añadiendo elementos (Enqueue) ---")
	cola.Enqueue("Cliente A")
	cola.Enqueue("Cliente B")
	cola.Enqueue("Cliente C")
	fmt.Println("Cola actual:", cola.items)
	peeked, ok := cola.Peek()
	if ok {
		fmt.Println("Elemento en el frente (Peek):", peeked)
	}
	fmt.Println("Tamaño de la cola:", cola.Size())

	fmt.Println("\n--- Atendiendo elementos (Dequeue) ---")	
	clienteAtendido, ok := cola.Dequeue()
	if ok {
		fmt.Println("Cliente atendido:", clienteAtendido)
	}
	fmt.Println("Cola actual:", cola.items)

	clienteAtendido, ok = cola.Dequeue()
	if ok {
		fmt.Println("Cliente atendido:", clienteAtendido)
	}
	fmt.Println("Cola actual:", cola.items)

	peeked, ok = cola.Peek()
	if ok {
		fmt.Println("Próximo cliente a atender:", peeked)
	} else {
		fmt.Println("La cola está vacía, no hay próximo cliente.")
	}
	fmt.Println("¿La cola está vacía?", cola.IsEmpty())

	// Intentar desencolar de una cola vacía
	fmt.Println("\nIntentando desencolar de una cola vacía:")
	_, ok = cola.Dequeue()
	if !ok {
		fmt.Println("Error: La cola está vacía.")
	}
}

func main() {
	QueueDemo()
}

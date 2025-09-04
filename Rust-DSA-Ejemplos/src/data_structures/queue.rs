//! Módulo para demostrar la implementación de una Cola (Queue) en Rust.

use std::collections::VecDeque;
use std::fmt;

// Definición de la estructura de la Cola
pub struct Queue<T> {
    items: VecDeque<T>,
}

// Implementación de métodos para la Cola
impl<T> Queue<T> {
    // Constructor para crear una nueva cola vacía
    pub fn new() -> Self {
        Queue {
            items: VecDeque::new(),
        }
    }

    // Comprueba si la cola está vacía
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    // Añade un elemento al final de la cola (enqueue)
    pub fn enqueue(&mut self, item: T) {
        self.items.push_back(item);
    }

    // Elimina y retorna el elemento del frente de la cola (dequeue)
    pub fn dequeue(&mut self) -> Option<T> {
        self.items.pop_front()
    }

    // Retorna el elemento del frente de la cola sin eliminarlo (peek)
    pub fn peek(&self) -> Option<&T> {
        self.items.front()
    }

    // Retorna el número de elementos en la cola
    pub fn size(&self) -> usize {
        self.items.len()
    }
}

// Implementación de Display para la Cola (para imprimirla fácilmente)
impl<T: fmt::Debug> fmt::Display for Queue<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.items)
    }
}

// Función de demostración para la Cola
pub fn queue_demo() {
    let mut cola = Queue::new();

    println!("--- INICIALIZANDO COLA ---");
    println!("¿La cola está vacía? {}", cola.is_empty());

    println!("\n--- Añadiendo elementos (enqueue) ---");
    cola.enqueue("Cliente A");
    cola.enqueue("Cliente B");
    cola.enqueue("Cliente C");
    println!("Cola actual: {}", cola);
    if let Some(item) = cola.peek() {
        println!("Elemento en el frente (peek): {}", item);
    }
    println!("Tamaño de la cola: {}", cola.size());

    println!("\n--- Atendiendo elementos (dequeue) ---");
    if let Some(cliente_atendido) = cola.dequeue() {
        println!("Cliente atendido: {}", cliente_atendido);
    }
    println!("Cola actual: {}", cola);

    if let Some(cliente_atendido) = cola.dequeue() {
        println!("Cliente atendido: {}", cliente_atendido);
    }
    println!("Cola actual: {}", cola);

    if let Some(item) = cola.peek() {
        println!("Próximo cliente a atender: {}", item);
    } else {
        println!("La cola está vacía, no hay próximo cliente.");
    }
    println!("¿La cola está vacía? {}", cola.is_empty());

    // Intentar desencolar de una cola vacía
    println!("\nIntentando desencolar de una cola vacía:");
    if cola.dequeue().is_none() {
        println!("Error: La cola está vacía.");
    }
}

//! Módulo para demostrar la implementación de una Lista Enlazada (Singly Linked List) en Rust.

use std::fmt;

// Definición de un Nodo
pub struct Node<T> {
    pub data: T,
    pub next: Option<Box<Node<T>>>, 
}

impl<T> Node<T> {
    pub fn new(data: T) -> Self {
        Node {
            data,
            next: None,
        }
    }
}

// Definición de la Lista Enlazada
pub struct LinkedList<T> {
    head: Option<Box<Node<T>>>, 
}

impl<T: PartialEq + fmt::Display + Copy> LinkedList<T> {
    // Constructor para crear una nueva lista enlazada vacía
    pub fn new() -> Self {
        LinkedList { head: None }
    }

    // Comprueba si la lista está vacía
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    // Añade un nodo al final de la lista
    pub fn append(&mut self, data: T) {
        let new_node = Box::new(Node::new(data));
        if self.is_empty() {
            self.head = Some(new_node);
            return;
        }

        let mut current = self.head.as_mut().unwrap();
        while let Some(ref mut next_node) = current.next {
            current = next_node;
        }
        current.next = Some(new_node);
    }

    // Añade un nodo al principio de la lista
    pub fn prepend(&mut self, data: T) {
        let mut new_node = Box::new(Node::new(data));
        new_node.next = self.head.take(); // Toma la cabeza actual y la asigna como siguiente
        self.head = Some(new_node);
    }

    // Elimina la primera ocurrencia de un nodo con el dato especificado
    pub fn delete(&mut self, data: T) {
        if self.is_empty() {
            return;
        }

        // Si el nodo a eliminar es la cabeza
        if self.head.as_ref().unwrap().data == data {
            self.head = self.head.take().unwrap().next;
            return;
        }

        let mut current = self.head.as_mut().unwrap();
        // Recorre hasta encontrar el nodo anterior al que se quiere eliminar
        while let Some(ref mut next_node) = current.next {
            if next_node.data == data {
                current.next = next_node.take().next; // Elimina el nodo
                return;
            }
            current = next_node;
        }
        println!("Dato '{}' no encontrado en la lista.", data);
    }

    // Muestra los elementos de la lista
    pub fn display(&self) {
        if self.is_empty() {
            println!("Lista vacía.");
            return;
        }
        let mut current = &self.head;
        let mut output = String::new();
        while let Some(node) = current {
            output.push_str(&format!("{} -> ", node.data));
            current = &node.next;
        }
        output.push_str("None");
        println!("{}", output);
    }
}

// Función de demostración para la Lista Enlazada
pub fn linked_list_demo() {
    let mut lista = LinkedList::new();
    println!("¿Lista vacía? {}", lista.is_empty());

    println!("\n--- Añadiendo elementos (append) ---");
    lista.append("A");
    lista.append("B");
    lista.append("C");
    lista.display();

    println!("\n--- Añadiendo al inicio (prepend) ---");
    lista.prepend("Inicio");
    lista.display();
    println!("¿Lista vacía? {}", lista.is_empty());

    println!("\n--- Eliminando elementos ---");
    lista.delete("B");
    println!("Después de eliminar 'B':");
    lista.display();

    lista.delete("Inicio");
    println!("Después de eliminar 'Inicio':");
    lista.display();

    lista.delete("Z"); // Intentar eliminar un elemento que no existe
    lista.display();
}

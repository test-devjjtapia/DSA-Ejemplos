package data_structures;

public class LinkedListDemo {
    Node head;

    public LinkedListDemo() {
        this.head = null;
    }

    public boolean isEmpty() {
        return head == null;
    }

    public void append(String data) {
        Node newNode = new Node(data);
        if (isEmpty()) {
            head = newNode;
            return;
        }

        Node lastNode = head;
        while (lastNode.next != null) {
            lastNode = lastNode.next;
        }
        lastNode.next = newNode;
    }

    public void prepend(String data) {
        Node newNode = new Node(data);
        newNode.next = head;
        head = newNode;
    }

    public void delete(String data) {
        if (isEmpty()) {
            return;
        }

        // Si el nodo a eliminar es la cabeza
        if (head.data.equals(data)) {
            head = head.next;
            return;
        }

        Node currentNode = head;
        while (currentNode.next != null && !currentNode.next.data.equals(data)) {
            currentNode = currentNode.next;
        }

        // Si se encontró el nodo a eliminar
        if (currentNode.next != null) {
            currentNode.next = currentNode.next.next;
        } else {
            System.out.println("Dato '" + data + "' no encontrado en la lista.");
        }
    }

    public void display() {
        if (isEmpty()) {
            System.out.println("Lista vacía.");
            return;
        }
        Node currentNode = head;
        StringBuilder sb = new StringBuilder();
        while (currentNode != null) {
            sb.append(currentNode.data).append(" -> ");
            currentNode = currentNode.next;
        }
        sb.append("None");
        System.out.println(sb.toString());
    }

    public static void main(String[] args) {
        LinkedListDemo lista = new LinkedListDemo();
        System.out.println("¿Lista vacía? " + lista.isEmpty());

        System.out.println("\n--- Añadiendo elementos (append) ---");
        lista.append("A");
        lista.append("B");
        lista.append("C");
        lista.display();

        System.out.println("\n--- Añadiendo al inicio (prepend) ---");
        lista.prepend("Inicio");
        lista.display();
        System.out.println("¿Lista vacía? " + lista.isEmpty());

        System.out.println("\n--- Eliminando elementos ---");
        lista.delete("B");
        System.out.println("Después de eliminar 'B':");
        lista.display();

        lista.delete("Inicio");
        System.out.println("Después de eliminar 'Inicio':");
        lista.display();

        lista.delete("Z"); // Intentar eliminar un elemento que no existe
        lista.display();
    }
}

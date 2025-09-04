package data_structures;

import java.util.LinkedList;
import java.util.Queue;

public class QueueDemo {

    public static void main(String[] args) {
        Queue<String> cola = new LinkedList<>();

        System.out.println("--- INICIALIZANDO COLA ---");
        System.out.println("¿La cola está vacía? " + cola.isEmpty());

        System.out.println("\n--- Añadiendo elementos (enqueue) ---");
        cola.offer("Cliente A"); // offer() es preferible a add() para colas
        cola.offer("Cliente B");
        cola.offer("Cliente C");
        System.out.println("Cola actual: " + cola);
        System.out.println("Elemento en el frente (peek): " + cola.peek());
        System.out.println("Tamaño de la cola: " + cola.size());

        System.out.println("\n--- Atendiendo elementos (dequeue) ---");
        String clienteAtendido = cola.poll(); // poll() es preferible a remove() para colas
        System.out.println("Cliente atendido: " + clienteAtendido);
        System.out.println("Cola actual: " + cola);

        clienteAtendido = cola.poll();
        System.out.println("Cliente atendido: " + clienteAtendido);
        System.out.println("Cola actual: " + cola);

        System.out.println("\nPróximo cliente a atender: " + cola.peek());
        System.out.println("¿La cola está vacía? " + cola.isEmpty());

        // Intentar desencolar de una cola vacía
        System.out.println("\nIntentando desencolar de una cola vacía:");
        cola.poll();
        cola.poll();
        System.out.println("Cliente atendido (de cola vacía): " + cola.poll()); // Retorna null
    }
}

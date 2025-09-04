package data_structures;

import java.util.Stack;

public class StackDemo {

    public static void main(String[] args) {
        Stack<String> pila = new Stack<>();

        System.out.println("--- INICIALIZANDO PILA ---");
        System.out.println("¿La pila está vacía? " + pila.isEmpty());

        System.out.println("\n--- Añadiendo elementos (push) ---");
        pila.push("Libro 1");
        pila.push("Libro 2");
        pila.push("Libro 3");
        System.out.println("Pila actual: " + pila);
        System.out.println("Elemento en la cima (peek): " + pila.peek());
        System.out.println("Tamaño de la pila: " + pila.size());

        System.out.println("\n--- Eliminando elementos (pop) ---");
        String elementoQuitado = pila.pop();
        System.out.println("Elemento quitado: " + elementoQuitado);
        System.out.println("Pila actual: " + pila);

        elementoQuitado = pila.pop();
        System.out.println("Elemento quitado: " + elementoQuitado);
        System.out.println("Pila actual: " + pila);

        System.out.println("\nElemento en la cima ahora: " + pila.peek());
        System.out.println("¿La pila está vacía? " + pila.isEmpty());

        // Intentar sacar de una pila vacía
        System.out.println("\nIntentando sacar de una pila vacía:");
        try {
            pila.pop();
            pila.pop();
            System.out.println("Elemento quitado (de pila vacía): " + pila.pop());
        } catch (java.util.EmptyStackException e) {
            System.out.println("Error: La pila está vacía. " + e.getMessage());
        }
    }
}

package data_structures;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ArrayDemo {

    public static void main(String[] args) {

        // --- 1. Creación y Acceso ---
        System.out.println("--- Creación y Acceso ---");
        // En Java, ArrayList es el equivalente a las listas dinámicas de Python
        List<String> frutas = new ArrayList<>(Arrays.asList("manzana", "banana", "cereza", "dátil"));
        System.out.println("Lista completa: " + frutas);

        // Acceso por índice (basado en 0)
        System.out.println("Primer elemento (índice 0): " + frutas.get(0));
        System.out.println("Tercer elemento (índice 2): " + frutas.get(2));
        System.out.println("Último elemento (índice " + (frutas.size() - 1) + "): " + frutas.get(frutas.size() - 1));

        // --- 2. Modificación ---
        System.out.println("\n--- Modificación ---");
        frutas.set(1, "arándano");
        System.out.println("Lista después de modificar el índice 1: " + frutas);

        // --- 3. Añadir elementos ---
        System.out.println("\n--- Añadir elementos ---");
        // Añadir al final
        frutas.add("frambuesa");
        System.out.println("Después de add('frambuesa'): " + frutas);

        // Insertar en una posición específica
        frutas.add(2, "kiwi"); // Inserta 'kiwi' en el índice 2
        System.out.println("Después de add(2, 'kiwi'): " + frutas);

        // --- 4. Eliminar elementos ---
        System.out.println("\n--- Eliminar elementos ---");
        // Eliminar el último elemento
        String elementoEliminado = frutas.remove(frutas.size() - 1);
        System.out.println("Elemento eliminado con remove(last_index): " + elementoEliminado);
        System.out.println("Lista actual: " + frutas);

        // Eliminar por índice
        String elementoEliminadoIndice = frutas.remove(1); // Elimina el elemento en el índice 1
        System.out.println("Elemento eliminado con remove(1): " + elementoEliminadoIndice);
        System.out.println("Lista actual: " + frutas);

        // Eliminar por valor (la primera ocurrencia)
        frutas.remove("cereza");
        System.out.println("Después de remove('cereza'): " + frutas);

        // --- 5. Slicing (Rebanado) ---
        System.out.println("\n--- Slicing ---");
        List<Integer> numeros = new ArrayList<>(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
        System.out.println("Lista de números: " + numeros);

        // Obtener un sub-arreglo (del índice 2 al 4)
        List<Integer> subLista = numeros.subList(2, 5); // subList(fromIndex, toIndex) - toIndex es exclusivo
        System.out.println("Sub-lista de [2:5]: " + subLista);

        // Desde el inicio hasta el índice 3
        List<Integer> desdeInicio = numeros.subList(0, 4);
        System.out.println("Sub-lista de [:4]: " + desdeInicio);

        // Desde el índice 5 hasta el final
        List<Integer> hastaFinal = numeros.subList(5, numeros.size());
        System.out.println("Sub-lista de [5:]: " + hastaFinal);
    }
}

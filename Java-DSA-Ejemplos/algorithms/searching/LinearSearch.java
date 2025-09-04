package algorithms.searching;

import java.util.Arrays;

public class LinearSearch {

    public static int linearSearch(int[] arr, int target) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == target) {
                return i; // Element found, return its index
            }
        }
        return -1; // Element not found
    }

    public static void main(String[] args) {
        int[] myList = {10, 50, 30, 70, 80, 20, 90, 40};

        System.out.println("Lista: " + Arrays.toString(myList));

        // --- Searching for an existing element ---
        int target1 = 80;
        int index1 = linearSearch(myList, target1);
        if (index1 != -1) {
            System.out.println("El elemento " + target1 + " se encuentra en el índice: " + index1);
        } else {
            System.out.println("El elemento " + target1 + " no se encontró en la lista.");
        }

        // --- Searching for a non-existing element ---
        int target2 = 100;
        int index2 = linearSearch(myList, target2);
        if (index2 != -1) {
            System.out.println("El elemento " + target2 + " se encuentra en el índice: " + index2);
        } else {
            System.out.println("El elemento " + target2 + " no se encontró en la lista.");
        }
    }
}

package algorithms.searching;

import java.util.Arrays;

public class BinarySearch {

    public static int binarySearch(int[] sortedArr, int target) {
        int low = 0;
        int high = sortedArr.length - 1;

        while (low <= high) {
            int mid = low + (high - low) / 2; // Avoids potential overflow
            int guess = sortedArr[mid];

            if (guess == target) {
                return mid; // Element found
            } else if (guess > target) {
                high = mid - 1;
            } else {
                low = mid + 1;
            }
        }
        return -1; // Element not found
    }

    public static void main(String[] args) {
        // The list MUST be sorted
        int[] sortedList = {2, 5, 8, 12, 16, 23, 38, 56, 72, 91};

        System.out.println("Lista ordenada: " + Arrays.toString(sortedList));

        // --- Searching for an existing element ---
        int target1 = 23;
        int index1 = binarySearch(sortedList, target1);
        if (index1 != -1) {
            System.out.println("El elemento " + target1 + " se encuentra en el índice: " + index1);
        } else {
            System.out.println("El elemento " + target1 + " no se encontró en la lista.");
        }

        // --- Searching for a non-existing element ---
        int target2 = 40;
        int index2 = binarySearch(sortedList, target2);
        if (index2 != -1) {
            System.out.println("El elemento " + target2 + " se encuentra en el índice: " + index2);
        } else {
            System.out.println("El elemento " + target2 + " no se encontró en la lista.");
        }
    }
}

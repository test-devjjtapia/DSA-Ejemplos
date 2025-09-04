package algorithms.sorting;

import java.util.Arrays;

public class SelectionSort {

    public static void selectionSort(int[] arr) {
        int n = arr.length;

        for (int i = 0; i < n - 1; i++) {
            int minIdx = i;
            for (int j = i + 1; j < n; j++) {
                if (arr[j] < arr[minIdx]) {
                    minIdx = j;
                }
            }
            // Swap the found minimum element with the first element
            int temp = arr[minIdx];
            arr[minIdx] = arr[i];
            arr[i] = temp;
        }
    }

    public static void main(String[] args) {
        int[] myList = {64, 25, 12, 22, 11};

        System.out.println("Lista desordenada: " + Arrays.toString(myList));

        selectionSort(myList);

        System.out.println("Lista ordenada:    " + Arrays.toString(myList));

        int[] anotherList = {38, 27, 43, 3, 9, 82, 10};
        System.out.println("\nLista desordenada: " + Arrays.toString(anotherList));
        selectionSort(anotherList);
        System.out.println("Lista ordenada:    " + Arrays.toString(anotherList));
    }
}

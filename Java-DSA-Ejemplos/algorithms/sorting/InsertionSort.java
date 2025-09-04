package algorithms.sorting;

import java.util.Arrays;

public class InsertionSort {

    public static void insertionSort(int[] arr) {
        int n = arr.length;
        for (int i = 1; i < n; i++) {
            int key = arr[i];
            int j = i - 1;

            // Move elements of arr[0..i-1], that are greater than key,
            // to one position ahead of their current position
            while (j >= 0 && arr[j] > key) {
                arr[j + 1] = arr[j];
                j = j - 1;
            }
            arr[j + 1] = key;
        }
    }

    public static void main(String[] args) {
        int[] myList = {12, 11, 13, 5, 6};

        System.out.println("Lista desordenada: " + Arrays.toString(myList));

        insertionSort(myList);

        System.out.println("Lista ordenada:    " + Arrays.toString(myList));

        int[] anotherList = {64, 34, 25, 12, 22, 11, 90};
        System.out.println("\nLista desordenada: " + Arrays.toString(anotherList));
        insertionSort(anotherList);
        System.out.println("Lista ordenada:    " + Arrays.toString(anotherList));
    }
}

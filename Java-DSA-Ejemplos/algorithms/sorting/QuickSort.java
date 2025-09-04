package algorithms.sorting;

import java.util.Arrays;

public class QuickSort {

    public static void quickSort(int[] arr) {
        if (arr == null || arr.length <= 1) {
            return;
        }
        quickSort(arr, 0, arr.length - 1);
    }

    private static void quickSort(int[] arr, int low, int high) {
        if (low < high) {
            int pi = partition(arr, low, high);

            quickSort(arr, low, pi - 1);
            quickSort(arr, pi + 1, high);
        }
    }

    private static int partition(int[] arr, int low, int high) {
        int pivot = arr[high];
        int i = (low - 1); // index of smaller element

        for (int j = low; j < high; j++) {
            // If current element is smaller than or equal to pivot
            if (arr[j] <= pivot) {
                i++;

                // swap arr[i] and arr[j]
                int temp = arr[i];
                arr[i] = arr[j];
                arr[j] = temp;
            }
        }

        // swap arr[i+1] and arr[high] (or pivot)
        int temp = arr[i + 1];
        arr[i + 1] = arr[high];
        arr[high] = temp;

        return i + 1;
    }

    public static void main(String[] args) {
        int[] myList = {10, 7, 8, 9, 1, 5};

        System.out.println("Lista desordenada: " + Arrays.toString(myList));

        quickSort(myList);

        System.out.println("Lista ordenada:    " + Arrays.toString(myList));

        int[] anotherList = {64, 34, 25, 12, 22, 11, 90};
        System.out.println("\nLista desordenada: " + Arrays.toString(anotherList));
        quickSort(anotherList);
        System.out.println("Lista ordenada:    " + Arrays.toString(anotherList));
    }
}

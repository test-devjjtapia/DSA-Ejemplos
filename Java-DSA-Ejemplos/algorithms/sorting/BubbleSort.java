package algorithms.sorting;

import java.util.Arrays;

public class BubbleSort {

    public static void bubbleSort(int[] arr) {
        int n = arr.length;
        for (int i = 0; i < n - 1; i++) {
            for (int j = 0; j < n - i - 1; j++) {
                if (arr[j] > arr[j + 1]) {
                    // swap arr[j] and arr[j+1]
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                }
            }
        }
    }

    public static void main(String[] args) {
        int[] myList = {64, 34, 25, 12, 22, 11, 90};

        System.out.println("Lista desordenada: " + Arrays.toString(myList));

        bubbleSort(myList);

        System.out.println("Lista ordenada:    " + Arrays.toString(myList));

        int[] anotherList = {5, 1, 4, 2, 8};
        System.out.println("\nLista desordenada: " + Arrays.toString(anotherList));
        bubbleSort(anotherList);
        System.out.println("Lista ordenada:    " + Arrays.toString(anotherList));
    }
}

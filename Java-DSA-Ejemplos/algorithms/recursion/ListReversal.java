package algorithms.recursion;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ListReversal {

    public static <T> List<T> reverseListRecursive(List<T> list) {
        // Base case: if the list is empty or has one element, it's already reversed.
        if (list.size() <= 1) {
            return new ArrayList<>(list);
        }

        // Recursive step:
        // 1. Take the first element (list.get(0))
        // 2. Recursively call the function with the rest of the list (list.subList(1, list.size()))
        // 3. Concatenate the first element to the end of the returned reversed list.
        List<T> reversedTail = reverseListRecursive(list.subList(1, list.size()));
        reversedTail.add(list.get(0));
        return reversedTail;
    }

    public static void main(String[] args) {
        List<Integer> myList = Arrays.asList(1, 2, 3, 4, 5);
        System.out.println("Lista original: " + myList);

        List<Integer> listaInvertida = reverseListRecursive(myList);
        System.out.println("Lista invertida: " + listaInvertida);

        // Example with another list
        List<String> anotherList = Arrays.asList("a", "b", "c", "d");
        System.out.println("\nLista original: " + anotherList);
        List<String> listaInvertida2 = reverseListRecursive(anotherList);
        System.out.println("Lista invertida: " + listaInvertida2);
    }
}

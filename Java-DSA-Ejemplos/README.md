# Proyecto de Estructuras de Datos y Algoritmos en Java

Este proyecto es un "fork" del repositorio original de ejemplos de Estructuras de Datos y Algoritmos (DSA) en Python, reimplementado en Java estándar (Core Java). El objetivo es proporcionar una colección clara y práctica de implementaciones de DSA utilizando las características y convenciones del lenguaje Java.

## Estructura del Proyecto

La estructura del proyecto Java replica la del proyecto original en Python para facilitar la comparación y comprensión:

```
Java-DSA-Ejemplos/
├── README.md
├── data_structures/
│   ├── ArrayDemo.java
│   ├── QueueDemo.java
│   ├── StackDemo.java
│   ├── Node.java
│   ├── LinkedListDemo.java
│   ├── TreeNode.java
│   └── BinarySearchTreeDemo.java
└── algorithms/
    ├── searching/
    │   ├── LinearSearch.java
    │   └── BinarySearch.java
    ├── sorting/
    │   ├── BubbleSort.java
    │   ├── SelectionSort.java
    │   ├── InsertionSort.java
    │   ├── MergeSort.java
    │   └── QuickSort.java
    └── recursion/
        ├── Fibonacci.java
        └── ListReversal.java
```

---

## Requisitos y Compilación

Este proyecto requiere el **Java Development Kit (JDK)**, versión 8 o superior. Puedes descargarlo desde el sitio oficial de Oracle o utilizar una distribución OpenJDK.

### Compilación y Ejecución

Para compilar y ejecutar los ejemplos, sigue estos pasos:

1.  **Navegar al directorio raíz del proyecto:**
    ```bash
    cd Java-DSA-Ejemplos
    ```

2.  **Compilar todos los archivos `.java`:**
    ```bash
    javac -d . data_structures/*.java algorithms/searching/*.java algorithms/sorting/*.java algorithms/recursion/*.java
    ```
    Este comando compilará todos los archivos fuente y colocará los archivos `.class` en sus respectivas estructuras de paquetes dentro del directorio actual.

3.  **Ejecutar un ejemplo específico:**
    Para ejecutar un programa, debes especificar su nombre de clase completamente calificado (incluyendo el paquete). Por ejemplo:

    ```bash
    java data_structures.ArrayDemo
    java algorithms.sorting.BubbleSort
    java algorithms.recursion.Fibonacci
    ```

---

## 🧠 Estructuras de Datos Implementadas

Las implementaciones en Java utilizan las clases y colecciones estándar del JDK cuando es apropiado, o se construyen desde cero para ilustrar los conceptos fundamentales.

| Estructura | Descripción | Implementación en Java |
| :--- | :--- | :--- |
| **Array (Arreglo)** | Colección secuencial de elementos. | `ArrayDemo.java`: Demuestra el uso de `java.util.ArrayList` para simular arrays dinámicos y operaciones básicas. |
| **Queue (Cola)** | Estructura FIFO (First-In, First-Out). | `QueueDemo.java`: Utiliza `java.util.LinkedList` (que implementa `java.util.Queue`) para operaciones de cola. |
| **Stack (Pila)** | Estructura LIFO (Last-In, First-Out). | `StackDemo.java`: Utiliza la clase `java.util.Stack` para operaciones de pila. |
| **Linked List (Lista Enlazada)** | Nodos conectados por referencias. | `Node.java` y `LinkedListDemo.java`: Implementación de una lista enlazada simple desde cero, con clases `Node` y `LinkedListDemo` para las operaciones. |
| **Tree (Árbol de Búsqueda Binaria)** | Estructura jerárquica con nodos y subárboles. | `TreeNode.java` y `BinarySearchTreeDemo.java`: Implementación de un BST desde cero, con clases `TreeNode` y `BinarySearchTreeDemo` para las operaciones de inserción, búsqueda y recorrido. |

---

## ⚙️ Algoritmos Implementados

Los algoritmos se implementan utilizando las características de Java, incluyendo la recursión nativa cuando es aplicable.

### Búsqueda (Searching)

| Algoritmo | Descripción | Implementación en Java |
| :--- | :--- | :--- |
| **Linear Search** | Recorre la colección elemento por elemento. | `LinearSearch.java`: Implementación directa de la búsqueda lineal. |
| **Binary Search** | Búsqueda eficiente en colecciones **ordenadas**. | `BinarySearch.java`: Implementación del algoritmo de búsqueda binaria. |

### Ordenamiento (Sorting)

| Algoritmo | Descripción | Implementación en Java |
| :--- | :--- | :--- |
| **Bubble Sort** | Compara e intercambia elementos adyacentes. | `BubbleSort.java`: Implementación del algoritmo de ordenamiento de burbuja. |
| **Selection Sort** | Encuentra el mínimo y lo coloca en su posición. | `SelectionSort.java`: Implementación del algoritmo de ordenamiento por selección. |
| **Insertion Sort** | Construye la lista ordenada insertando elementos. | `InsertionSort.java`: Implementación del algoritmo de ordenamiento por inserción. |
| **Merge Sort** | Algoritmo "Divide y Vencerás" que fusiona sub-listas. | `MergeSort.java`: Implementación recursiva del algoritmo de ordenamiento por mezcla. |
| **Quick Sort** | Algoritmo "Divide y Vencerás" que particiona la lista. | `QuickSort.java`: Implementación recursiva del algoritmo de ordenamiento rápido. |

### Recursión

Java soporta la recursión de forma nativa, lo que permite implementaciones directas de algoritmos recursivos.

| Concepto | Descripción | Implementación en Java |
| :--- | :--- | :--- |
| **Fibonacci** | Cálculo de la secuencia de Fibonacci. | `Fibonacci.java`: Incluye implementaciones recursivas (ingenua y con memoización) e iterativas. |
| **Inversión de Lista** | Invierte una lista. | `ListReversal.java`: Implementación recursiva para invertir una lista. |

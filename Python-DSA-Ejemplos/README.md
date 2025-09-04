# Proyecto de Estructuras de Datos y Algoritmos en Python

Este proyecto contiene ejemplos prácticos de las estructuras de datos y algoritmos más comunes, implementados en Python. El objetivo es servir como una guía de estudio y referencia rápida.

Cada concepto está en su propio archivo para mayor claridad. Puedes ejecutar cada archivo individualmente para ver los ejemplos en acción.

## Estructura del Proyecto

```
/
├── README.md
├── data_structures/
│   ├── array.py
│   ├── queue.py
│   ├── stack.py
│   ├── linked_list.py
│   ├── tree.py
│   ├── graph.py
│   └── ...
└── algorithms/
    ├── searching/
    │   ├── linear_search.py
    │   └── binary_search.py
    └── sorting/
        ├── bubble_sort.py
        ├── quick_sort.py
        └── ...
```

---

## 🧠 Estructuras de Datos

Una estructura de datos es una forma de organizar, gestionar y almacenar datos para permitir un acceso y una modificación eficientes.

| Estructura | Descripción | Código de Ejemplo |
| :--- | :--- | :--- |
| **Array (Arreglo)** | Almacena elementos en una lista secuencial. En Python, las `list` son arreglos dinámicos. | [`Ver Código`](./data_structures/array.py) |
| **Queue (Cola)** | Estructura FIFO (primero en entrar, primero en salir). | [`Ver Código`](./data_structures/queue.py) |
| **Stack (Pila)** | Estructura LIFO (último en entrar, primero en salir). | [`Ver Código`](./data_structures/stack.py) |
| **Linked List (Lista Enlazada)** | Nodos conectados por punteros. Eficiente para inserciones/eliminaciones. | [`Ver Código`](./data_structures/linked_list.py) |
| **Tree (Árbol)** | Estructura jerárquica con un nodo raíz y nodos hijos. | [`Ver Código`](./data_structures/tree.py) |
| **Matrix (Matriz)** | Disposición de datos en formato de rejilla (array 2D). | [`Ver Código`](./data_structures/matrix.py) |
| **HashMap (Tabla Hash)** | Almacena pares clave-valor. En Python se implementa con `dict`. | [`Ver Código`](./data_structures/hash_map.py) |
| **BST (Árbol de Búsqueda Binaria)** | Árbol binario ordenado que permite búsquedas rápidas. | [`Ver Código`](./data_structures/tree.py) |
| **Heap (Montículo)** | Árbol binario usado para colas de prioridad. | [`Ver Código`](./data_structures/heap.py) |
| **Trie (Árbol de prefijos)** | Eficiente para almacenar y buscar cadenas (ej. autocompletado). | [`Ver Código`](./data_structures/trie.py) |
| **Graph (Grafo)** | Conjunto de nodos (vértices) y aristas (conexiones). | [`Ver Código`](./data_structures/graph.py) |
| **Union-Find** | Gestiona y fusiona conjuntos disjuntos. | [`Ver Código`](./data_structures/union_find.py) |

---

## ⚙️ Algoritmos

Un algoritmo es un conjunto de instrucciones paso a paso para realizar un cálculo o resolver un problema.

### Búsqueda (Searching)

| Algoritmo | Descripción | Código de Ejemplo |
| :--- | :--- | :--- |
| **Linear Search** | Recorre una colección elemento por elemento. Simple pero ineficiente. | [`Ver Código`](./algorithms/searching/linear_search.py) |
| **Binary Search** | Busca eficientemente en una colección **ordenada** dividiendo el rango de búsqueda por la mitad. | [`Ver Código`](./algorithms/searching/binary_search.py) |

### Ordenamiento (Sorting)

| Algoritmo | Descripción | Código de Ejemplo |
| :--- | :--- | :--- |
| **Bubble Sort** | Compara e intercambia repetidamente elementos adyacentes si están en el orden incorrecto. | [`Ver Código`](./algorithms/sorting/bubble_sort.py) |
| **Selection Sort** | Selecciona el elemento más pequeño y lo mueve al principio de la lista. | [`Ver Código`](./algorithms/sorting/selection_sort.py) |
| **Insertion Sort** | Construye la lista ordenada final un elemento a la vez. | [`Ver Código`](./algorithms/sorting/insertion_sort.py) |
| **Merge Sort** | Algoritmo "Divide y Vencerás". Divide la lista, la ordena y luego la fusiona. | [`Ver Código`](./algorithms/sorting/merge_sort.py) |
| **Quick Sort** | Algoritmo "Divide y Vencerás". Elige un "pivote" y particiona la lista a su alrededor. | [`Ver Código`](./algorithms/sorting/quick_sort.py) |

### Recursión

La recursión es una técnica en la que una función se llama a sí misma para resolver subproblemas más pequeños.

| Concepto | Descripción | Código de Ejemplo |
| :--- | :--- | :--- |
| **Fibonacci** | Ejemplo clásico para demostrar recursión (y sus posibles ineficiencias). | [`Ver Código`](./algorithms/recursion/fibonacci.py) |
| **Inversión de Lista** | Invierte una lista usando un enfoque recursivo. | [`Ver Código`](./algorithms/recursion/list_reversal.py) |

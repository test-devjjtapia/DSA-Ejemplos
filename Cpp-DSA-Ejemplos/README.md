# Proyecto de Estructuras de Datos y Algoritmos en C++

Este proyecto es un "fork" del repositorio original de ejemplos de Estructuras de Datos y Algoritmos (DSA) en Python, reimplementado en C++. El objetivo es proporcionar una colección clara y práctica de implementaciones de DSA utilizando las características y convenciones del lenguaje C++.

## Desafíos de Implementación en C++

C++ es un lenguaje potente que permite un control directo sobre la memoria, lo que lo hace ideal para la implementación de estructuras de datos y algoritmos de bajo nivel. Sin embargo, este control conlleva la responsabilidad de la gestión de memoria.

-   **Gestión de Memoria:** A diferencia de Python o Java con sus recolectores de basura, en C++ la gestión de memoria es manual. Esto significa que se debe asignar memoria (`new`) y liberarla (`delete`) explícitamente para evitar fugas de memoria. Para estructuras de datos complejas como listas enlazadas y árboles, esto es crucial.
-   **Punteros:** C++ hace un uso extensivo de punteros para construir estructuras de datos dinámicas. Es fundamental entender cómo funcionan los punteros y las referencias para manipular correctamente estas estructuras.
-   **Plantillas (Templates):** C++ permite el uso de plantillas para escribir código genérico que funcione con diferentes tipos de datos, lo que aumenta la reutilización y la flexibilidad de las implementaciones de DSA.
-   **Librería Estándar (STL):** C++ cuenta con una rica Librería de Plantillas Estándar (STL) que proporciona implementaciones eficientes de muchas estructuras de datos (como `std::vector`, `std::queue`, `std::stack`, `std::map`) y algoritmos. Este proyecto utiliza la STL cuando es apropiado para demostrar su uso, pero también implementa algunas estructuras desde cero para fines educativos.

## Estructura del Proyecto

La estructura del proyecto C++ replica la del proyecto original en Python para facilitar la comparación y comprensión:

```
Cpp-DSA-Ejemplos/
├── README.md
├── data_structures/
│   ├── array_demo.cpp
│   ├── queue_demo.cpp
│   ├── stack_demo.cpp
│   ├── linked_list_demo.cpp
│   └── binary_search_tree_demo.cpp
└── algorithms/
    ├── searching/
    │   ├── linear_search.cpp
    │   └── binary_search.cpp
    ├── sorting/
    │   ├── bubble_sort.cpp
    │   ├── selection_sort.cpp
    │   ├── insertion_sort.cpp
    │   ├── merge_sort.cpp
    │   └── quick_sort.cpp
    └── recursion/
        ├── fibonacci.cpp
        └── list_reversal.cpp
```

## Requisitos y Compilación

Este proyecto requiere un compilador de C++ compatible con C++11 o superior (por ejemplo, GCC, Clang, MSVC).

### Compilación y Ejecución

Para compilar y ejecutar los ejemplos, puedes usar un compilador como `g++` (parte de GCC) en sistemas tipo Unix (Linux, macOS) o MinGW/MSYS2 en Windows, o Visual Studio.

**Ejemplo con g++ (Linux/macOS/MinGW):**

1.  **Navegar al directorio del archivo:**
    ```bash
    cd Cpp-DSA-Ejemplos/data_structures
    # o cd Cpp-DSA-Ejemplos/algorithms/sorting
    ```

2.  **Compilar el archivo:**
    ```bash
    g++ -std=c++11 -o array_demo array_demo.cpp
    # o g++ -std=c++11 -o bubble_sort bubble_sort.cpp
    ```
    -   `-std=c++11`: Especifica el estándar C++11 (o `c++14`, `c++17`, `c++20` para versiones más recientes).
    -   `-o <nombre_ejecutable>`: Especifica el nombre del archivo de salida ejecutable.

3.  **Ejecutar el programa:**
    ```bash
    ./array_demo
    # o ./bubble_sort
    ```

---

## 🧠 Estructuras de Datos Implementadas

Las implementaciones en C++ utilizan tanto las clases de la STL como implementaciones personalizadas para ilustrar los conceptos.

| Estructura | Descripción | Implementación en C++ |
| :--- | :--- | :--- |
| **Array (Arreglo)** | Colección secuencial de elementos. | `array_demo.cpp`: Demuestra el uso de `std::vector` para arrays dinámicos y sus operaciones. |
| **Queue (Cola)** | Estructura FIFO (First-In, First-Out). | `queue_demo.cpp`: Utiliza `std::queue` (adaptador de contenedor) para las operaciones de cola. |
| **Stack (Pila)** | Estructura LIFO (Last-In, First-Out). | `stack_demo.cpp`: Utiliza `std::stack` (adaptador de contenedor) para las operaciones de pila. |
| **Linked List (Lista Enlazada)** | Nodos conectados por referencias. | `linked_list_demo.cpp`: Implementación de una lista enlazada simple desde cero, utilizando punteros (`Node*`) y gestión manual de memoria (`new`, `delete`). Incluye un destructor para evitar fugas de memoria. |
| **Tree (Árbol de Búsqueda Binaria)** | Estructura jerárquica con nodos y subárboles. | `binary_search_tree_demo.cpp`: Implementación de un BST desde cero, utilizando punteros (`TreeNode*`) y gestión manual de memoria. Incluye inserción, búsqueda y recorrido en orden. |

---

## ⚙️ Algoritmos Implementados

Los algoritmos se implementan aprovechando la eficiencia y el control que ofrece C++.

### Búsqueda (Searching)

| Algoritmo | Descripción | Implementación en C++ |
| :--- | :--- | :--- |
| **Linear Search** | Recorre la colección elemento por elemento. | `linear_search.cpp`: Implementación directa de la búsqueda lineal en un `std::vector`. |
| **Binary Search** | Búsqueda eficiente en colecciones **ordenadas**. | `binary_search.cpp`: Implementación del algoritmo de búsqueda binaria en un `std::vector` ordenado. |

### Ordenamiento (Sorting)

| Algoritmo | Descripción | Implementación en C++ |
| :--- | :--- | :--- |
| **Bubble Sort** | Compara e intercambia elementos adyacentes. | `bubble_sort.cpp`: Implementación del algoritmo de ordenamiento de burbuja en un `std::vector` mutable. |
| **Selection Sort** | Encuentra el mínimo y lo coloca en su posición. | `selection_sort.cpp`: Implementación del algoritmo de ordenamiento por selección en un `std::vector` mutable. |
| **Insertion Sort** | Construye la lista ordenada insertando elementos. | `insertion_sort.cpp`: Implementación del algoritmo de ordenamiento por inserción en un `std::vector` mutable. |
| **Merge Sort** | Algoritmo "Divide y Vencerás" que fusiona sub-listas. | `merge_sort.cpp`: Implementación recursiva del algoritmo de ordenamiento por mezcla. |
| **Quick Sort** | Algoritmo "Divide y Vencerás" que particiona la lista. | `quick_sort.cpp`: Implementación recursiva del algoritmo de ordenamiento rápido. |

### Recursión

C++ soporta la recursión de forma nativa, lo que permite implementaciones directas de algoritmos recursivos.

| Concepto | Descripción | Implementación en C++ |
| :--- | :--- | :--- |
| **Fibonacci** | Cálculo de la secuencia de Fibonacci. | `fibonacci.cpp`: Incluye implementaciones recursivas (ingenua y con memoización) e iterativas. |
| **Inversión de Lista** | Invierte una lista. | `list_reversal.cpp`: Implementación recursiva para invertir un `std::vector`. |

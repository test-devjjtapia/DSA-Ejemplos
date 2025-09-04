# Proyecto de Estructuras de Datos y Algoritmos en Go

Este proyecto es un "fork" del repositorio original de ejemplos de Estructuras de Datos y Algoritmos (DSA) en Python, reimplementado en Go. El objetivo es proporcionar una colección clara y práctica de implementaciones de DSA utilizando las características y convenciones del lenguaje Go.

## Estructura del Proyecto

La estructura del proyecto Go replica la del proyecto original en Python para facilitar la comparación y comprensión:

```
Go-DSA-Ejemplos/
├── README.md
├── data_structures/
│   ├── array.go
│   ├── queue.go
│   ├── stack.go
│   ├── linked_list.go
│   └── binary_search_tree.go
└── algorithms/
    ├── searching/
    │   ├── linear_search.go
    │   └── binary_search.go
    ├── sorting/
    │   ├── bubble_sort.go
    │   ├── selection_sort.go
    │   ├── insertion_sort.go
    │   ├── merge_sort.go
    │   └── quick_sort.go
    └── recursion/
        ├── fibonacci.go
        └── list_reversal.go
```

---

## Requisitos y Compilación

Este proyecto requiere el **Go SDK** (Go 1.16 o superior).

### Instalación de Go

Visita la página oficial de Go para descargar e instalar el SDK:
[https://golang.org/doc/install](https://golang.org/doc/install)

### Compilación y Ejecución

Cada archivo `.go` en este proyecto está diseñado para ser un programa ejecutable independiente. Para compilar y ejecutar un ejemplo:

1.  **Navega al directorio del archivo:**
    ```bash
    cd Go-DSA-Ejemplos/data_structures
    # o cd Go-DSA-Ejemplos/algorithms/sorting
    ```

2.  **Ejecuta el archivo:**
    ```bash
    go run array.go
    # o go run bubble_sort.go
    ```

Alternativamente, puedes compilar un ejecutable:

```bash
go build -o mi_programa array.go
./mi_programa
```

---

## 🧠 Estructuras de Datos Implementadas

Las implementaciones en Go utilizan las características del lenguaje como slices, structs y punteros para construir las estructuras de datos.

| Estructura | Descripción | Implementación en Go |
| :--- | :--- | :--- |
| **Array (Arreglo)** | Colección secuencial de elementos. | `array.go`: Demuestra el uso de slices para simular arrays dinámicos y operaciones básicas como añadir, eliminar y rebanar. |
| **Queue (Cola)** | Estructura FIFO (First-In, First-Out). | `queue.go`: Implementa una cola utilizando un slice subyacente y métodos para `Enqueue`, `Dequeue`, `Peek`, `IsEmpty` y `Size`. |
| **Stack (Pila)** | Estructura LIFO (Last-In, First-Out). | `stack.go`: Implementa una pila utilizando un slice subyacente y métodos para `Push`, `Pop`, `Peek`, `IsEmpty` y `Size`. |
| **Linked List (Lista Enlazada)** | Nodos conectados por referencias. | `linked_list.go`: Implementación de una lista enlazada simple desde cero, utilizando `struct` para los nodos y punteros para las conexiones. |
| **Tree (Árbol de Búsqueda Binaria)** | Estructura jerárquica con nodos y subárboles. | `binary_search_tree.go`: Implementación de un BST desde cero, utilizando `struct` para los nodos y punteros para los hijos izquierdo y derecho. Incluye inserción, búsqueda y recorrido en orden. |

---

## ⚙️ Algoritmos Implementados

Los algoritmos se implementan utilizando las características de Go, incluyendo la recursión nativa.

### Búsqueda (Searching)

| Algoritmo | Descripción | Implementación en Go |
| :--- | :--- | :--- |
| **Linear Search** | Recorre la colección elemento por elemento. | `linear_search.go`: Implementación directa de la búsqueda lineal. |
| **Binary Search** | Búsqueda eficiente en colecciones **ordenadas**. | `binary_search.go`: Implementación del algoritmo de búsqueda binaria. |

### Ordenamiento (Sorting)

| Algoritmo | Descripción | Implementación en Go |
| :--- | :--- | :--- |
| **Bubble Sort** | Compara e intercambia elementos adyacentes. | `bubble_sort.go`: Implementación del algoritmo de ordenamiento de burbuja. |
| **Selection Sort** | Encuentra el mínimo y lo coloca en su posición. | `selection_sort.go`: Implementación del algoritmo de ordenamiento por selección. |
| **Insertion Sort** | Construye la lista ordenada insertando elementos. | `insertion_sort.go`: Implementación del algoritmo de ordenamiento por inserción. |
| **Merge Sort** | Algoritmo "Divide y Vencerás" que fusiona sub-listas. | `merge_sort.go`: Implementación recursiva del algoritmo de ordenamiento por mezcla. |
| **Quick Sort** | Algoritmo "Divide y Vencerás" que particiona la lista. | `quick_sort.go`: Implementación recursiva del algoritmo de ordenamiento rápido. |

### Recursión

Go soporta la recursión de forma nativa, lo que permite implementaciones directas de algoritmos recursivos.

| Concepto | Descripción | Implementación en Go |
| :--- | :--- | :--- |
| **Fibonacci** | Cálculo de la secuencia de Fibonacci. | `fibonacci.go`: Incluye implementaciones recursivas (ingenua y con memoización) e iterativas. |
| **Inversión de Lista** | Invierte una lista. | `list_reversal.go`: Implementación recursiva para invertir una lista. |

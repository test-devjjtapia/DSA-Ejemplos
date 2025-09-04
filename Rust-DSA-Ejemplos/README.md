# Proyecto de Estructuras de Datos y Algoritmos en Rust

Este proyecto es un "fork" del repositorio original de ejemplos de Estructuras de Datos y Algoritmos (DSA) en Python, reimplementado en Rust. El objetivo es proporcionar una colección clara y práctica de implementaciones de DSA utilizando las características y el paradigma de seguridad de memoria de Rust.

## Desafíos de Implementación en Rust

Rust es un lenguaje que ofrece seguridad de memoria sin un recolector de basura, lo que introduce conceptos como la propiedad (ownership), el préstamo (borrowing) y las duraciones (lifetimes). Esto hace que la implementación de ciertas estructuras de datos, especialmente aquellas con referencias cíclicas o mutabilidad compartida, sea más compleja que en otros lenguajes.

-   **Propiedad y Préstamo:** La gestión de la memoria se realiza a través del sistema de propiedad de Rust, lo que significa que cada valor tiene una variable "propietaria" y solo puede haber un propietario a la vez. Esto requiere un diseño cuidadoso al construir estructuras de datos que implican referencias.
-   **`Option` y `Box`:** Para estructuras de datos como listas enlazadas y árboles, se utiliza `Option<T>` para representar la ausencia de un valor (como `None` en Python o `null` en Java) y `Box<T>` para asignar datos en el heap, lo que permite tamaños dinámicos y recursividad.
-   **`Rc` y `RefCell`:** Para estructuras de datos más complejas que requieren múltiples propietarios o mutabilidad interior (modificar un valor a través de una referencia inmutable), se utilizan `Rc<T>` (Reference Counting) y `RefCell<T>` (Runtime Borrow Checking). Estos añaden un costo en tiempo de ejecución pero permiten flexibilidad.

## Estructura del Proyecto

La estructura del proyecto Rust sigue las convenciones de Cargo (el gestor de paquetes de Rust) y replica la organización del proyecto original:

```
Rust-DSA-Ejemplos/
├── Cargo.toml
├── src/
│   ├── main.rs
│   ├── data_structures/
│   │   ├── array.rs
│   │   ├── queue.rs
│   │   ├── stack.rs
│   │   ├── linked_list.rs
│   │   └── binary_search_tree.rs
│   └── algorithms/
│       ├── searching/
│       │   ├── linear_search.rs
│       │   └── binary_search.rs
│       ├── sorting/
│       │   ├── bubble_sort.rs
│       │   ├── selection_sort.rs
│       │   ├── insertion_sort.rs
│       │   ├── merge_sort.rs
│       │   └── quick_sort.rs
│       └── recursion/
│           ├── fibonacci.rs
│           └── list_reversal.rs
└── README.md
```

**Nota:** Los comentarios en el código fuente (`.rs` files) están en español, como se solicitó.

---

## Requisitos y Compilación

Este proyecto requiere el **Rust toolchain** (compilador `rustc` y gestor de paquetes `cargo`).

### Instalación de Rust

La forma recomendada de instalar Rust es a través de `rustup`:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Para Windows, descarga el instalador `rustup-init.exe` desde [https://rustup.rs/](https://rustup.rs/).

### Compilación y Ejecución

1.  **Navegar al directorio raíz del proyecto:**
    ```bash
    cd Rust-DSA-Ejemplos
    ```

2.  **Compilar el proyecto:**
    ```bash
    cargo build
    ```
    Esto compilará todos los módulos y creará un ejecutable en `target/debug/rust-dsa-ejemplos`.

3.  **Ejecutar el proyecto:**
    ```bash
    cargo run
    ```
    Por defecto, `main.rs` está configurado para llamar a una función de demostración específica. Para ejecutar una demostración diferente, edita `src/main.rs` y descomenta la línea de la función que deseas ejecutar y comenta las demás.

4.  **Ejecutar pruebas (si las hubiera):**
    ```bash
    cargo test
    ```

---

## 🧠 Estructuras de Datos Implementadas

Las implementaciones en Rust utilizan las características del lenguaje para garantizar la seguridad de memoria.

| Estructura | Descripción | Implementación en Rust |
| :--- | :--- | :--- |
| **Array (Arreglo)** | Colección secuencial de elementos. | `array.rs`: Demuestra el uso de `Vec<T>` (vector dinámico) y sus métodos para operaciones básicas como añadir, eliminar y rebanar. |
| **Queue (Cola)** | Estructura FIFO (First-In, First-Out). | `queue.rs`: Implementa una cola utilizando `std::collections::VecDeque`, optimizada para operaciones en ambos extremos. |
| **Stack (Pila)** | Estructura LIFO (Last-In, First-Out). | `stack.rs`: Implementa una pila utilizando `Vec<T>` como estructura subyacente. |
| **Linked List (Lista Enlazada)** | Nodos conectados por referencias. | `linked_list.rs`: Implementación de una lista enlazada simple utilizando `Option<Box<Node<T>>>` para manejar la propiedad y la recursividad de los nodos. Es un ejemplo de la complejidad de Rust para estructuras de datos con referencias. |
| **Tree (Árbol de Búsqueda Binaria)** | Estructura jerárquica con nodos y subárboles. | `binary_search_tree.rs`: Implementación de un BST utilizando `Option<Box<TreeNode>>` para los nodos hijos, con inserción, búsqueda y recorrido en orden implementados recursivamente. |

---

## ⚙️ Algoritmos Implementados

Los algoritmos se implementan aprovechando la seguridad y el rendimiento de Rust.

### Búsqueda (Searching)

| Algoritmo | Descripción | Implementación en Rust |
| :--- | :--- | :--- |
| **Linear Search** | Recorre la colección elemento por elemento. | `linear_search.rs`: Implementación directa de la búsqueda lineal en un slice. |
| **Binary Search** | Búsqueda eficiente en colecciones **ordenadas**. | `binary_search.rs`: Implementación del algoritmo de búsqueda binaria en un slice ordenado. |

### Ordenamiento (Sorting)

| Algoritmo | Descripción | Implementación en Rust |
| :--- | :--- | :--- |
| **Bubble Sort** | Compara e intercambia elementos adyacentes. | `bubble_sort.rs`: Implementación del algoritmo de ordenamiento de burbuja en un slice mutable. |
| **Selection Sort** | Encuentra el mínimo y lo coloca en su posición. | `selection_sort.rs`: Implementación del algoritmo de ordenamiento por selección en un slice mutable. |
| **Insertion Sort** | Construye la lista ordenada insertando elementos. | `insertion_sort.rs`: Implementación del algoritmo de ordenamiento por inserción en un slice mutable. |
| **Merge Sort** | Algoritmo "Divide y Vencerás" que fusiona sub-listas. | `merge_sort.rs`: Implementación recursiva del algoritmo de ordenamiento por mezcla. |
| **Quick Sort** | Algoritmo "Divide y Vencerás" que particiona la lista. | `quick_sort.rs`: Implementación recursiva del algoritmo de ordenamiento rápido. |

### Recursión

Rust soporta la recursión de forma nativa, lo que permite implementaciones directas de algoritmos recursivos.

| Concepto | Descripción | Implementación en Rust |
| :--- | | :--- |
| **Fibonacci** | Cálculo de la secuencia de Fibonacci. | `fibonacci.rs`: Incluye implementaciones recursivas (ingenua y con memoización) e iterativas. |
| **Inversión de Lista** | Invierte una lista. | `list_reversal.rs`: Implementación recursiva para invertir una lista. |

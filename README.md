# 📖 Estructuras de Datos y Algoritmos (DSA)

¡Bienvenido a este repositorio de Estructuras de Datos y Algoritmos!

Dominar DSA es una habilidad fundamental para cualquier desarrollador de software. No solo es clave para superar entrevistas técnicas, sino también para resolver problemas complejos del mundo real, optimizar el rendimiento de las aplicaciones y construir sistemas escalables y eficientes.

## 🤔 ¿Qué son las Estructuras de Datos y Algoritmos?

-   Una **Estructura de Datos** es una forma de organizar, gestionar y almacenar datos para permitir un acceso y una modificación eficientes.
-   Un **Algoritmo** es un conjunto de instrucciones paso a paso, como una receta, diseñado para realizar un cálculo o resolver un problema específico.

La elección de la estructura de datos y el algoritmo correctos puede cambiar drásticamente el rendimiento y la eficiencia de un programa.

---

## 🚀 Conceptos Fundamentales

### Bucles vs. Recursión

Los algoritmos se pueden implementar principalmente de dos maneras:

1.  **Bucles (Iteración):** Repiten un conjunto de instrucciones hasta que se cumple una condición de parada. Son predecibles y, a menudo, más eficientes en el uso de memoria.
2.  **Recursión:** Una función se llama a sí misma con una versión más pequeña del problema hasta que se alcanza un "caso base". Ofrece soluciones elegantes para problemas que se pueden subdividir, pero puede consumir más memoria (riesgo de *stack overflow*).

#### Ejemplo Práctico: Secuencia de Fibonacci

La elección entre bucles y recursión tiene un impacto directo en el rendimiento.

-   **Enfoque Recursivo (Ingenuo):** `fib(n) = fib(n-1) + fib(n-2)`. Cada llamada genera dos más, lo que conduce a un crecimiento exponencial y se vuelve extremadamente lento para valores grandes de `n`.
-   **Enfoque Iterativo (Bucle):** Utiliza un simple bucle `for` o `while` para calcular la secuencia. Se ejecuta en tiempo lineal (O(n)), lo que lo hace mucho más rápido y eficiente.
-   **Recursión con Memoización:** Una versión optimizada de la recursión que almacena los resultados ya calculados para evitar cómputos repetidos, combinando la elegancia recursiva con una eficiencia similar a la de los bucles.

> **💡 Lección Clave:** Prefiere los bucles cuando la eficiencia y el uso de memoria son críticos. La recursión puede ser muy elegante, pero debe usarse con cuidado, idealmente con optimizaciones como la memoización.

---

## 🗂️ Estructuras de Datos Esenciales

A continuación se presentan las estructuras de datos más comunes que todo programador debería conocer.

| Icono | Estructura | Descripción | Casos de Uso Comunes |
| :---: | :--- | :--- | :--- |
| 📦 | **Array (Arreglo)** | Colección secuencial de elementos con acceso rápido por índice. | Acceso aleatorio a datos, listas de tamaño fijo. |
| 📥 | **Queue (Cola, FIFO)** | Estructura **F**irst-**I**n, **F**irst-**O**ut (primero en entrar, primero en salir). | Planificación de tareas, Búsqueda en Anchura (BFS), buffers. |
| 📤 | **Stack (Pila, LIFO)** | Estructura **L**ast-**I**n, **F**irst-**O**ut (último en entrar, primero en salir). | Funciones de deshacer/rehacer, gestión de llamadas a funciones, parsing. |
| 🔗 | **Linked List (Lista Enlazada)** | Nodos conectados por punteros, permitiendo inserciones/eliminaciones eficientes. | Implementación de otras estructuras (pilas, colas), listas dinámicas. |
| 🔑 | **HashMap (Tabla Hash)** | Pares clave-valor con acceso casi instantáneo (O(1) en promedio) a través de una función hash. | Caché, diccionarios, conteo de frecuencias, búsquedas rápidas. |
| 🌳 | **Tree (Árbol)** | Estructura jerárquica con un nodo raíz y relaciones padre-hijo. | Sistemas de archivos, DOM en HTML, análisis sintáctico (parsing). |
| 🌲 | **Binary Search Tree (BST)** | Árbol binario ordenado (`izquierda < raíz < derecha`) que permite búsquedas, inserciones y eliminaciones rápidas (O(log n)). | Búsquedas eficientes, implementaciones de `sets` y `maps`. |
| 🏔️ | **Heap (Montículo)** | Árbol binario que satisface la "propiedad del heap" (Min-Heap o Max-Heap). | Colas de prioridad, algoritmo de ordenamiento Heapsort. |
| 🔗 | **Graph (Grafo)** | Conjunto de nodos (vértices) y aristas (conexiones) que los unen. | Redes sociales, mapas (GPS), modelado de redes, dependencias. |
| 🔤 | **Trie (Árbol de Prefijos)** | Árbol de búsqueda de cadenas eficiente para tareas de autocompletado y búsqueda en diccionarios. | Autocompletado, correctores ortográficos, enrutamiento de IP. |
| 🔢 | **Matrix (Matriz)** | Disposición de datos en formato de rejilla (array 2D). | Procesamiento de imágenes, juegos (tableros), programación dinámica. |
| 🧩 | **Union-Find** | Gestiona conjuntos de elementos disjuntos y permite fusionarlos eficientemente. | Detección de ciclos en grafos, algoritmo de Kruskal, redes de conexión. |

---

## ⚙️ Algoritmos Fundamentales

### Conceptos Clave

-   **Búsqueda (Searching):** Encontrar la ubicación de un elemento.
    -   *Búsqueda Lineal:* Simple, pero lento (O(n)).
    -   *Búsqueda Binaria:* Muy rápido (O(log n)), pero requiere que los datos estén ordenados.
-   **Ordenamiento (Sorting):** Organizar elementos en un orden específico.
    -   *Simples:* Bubble Sort, Selection Sort, Insertion Sort (O(n²)).
    -   *Avanzados:* Merge Sort, Quick Sort (O(n log n)).
-   **Divide y Vencerás:** Dividir un problema en subproblemas más pequeños, resolverlos y combinar los resultados (ej. Merge Sort, Quick Sort).
-   **Programación Dinámica:** Resolver problemas complejos dividiéndolos en subproblemas superpuestos y almacenando los resultados para evitar recálculos (memoización y tabulación).
-   **Algoritmos Voraces (Greedy):** Tomar la decisión localmente óptima en cada paso con la esperanza de encontrar una solución globalmente óptima.

### Técnicas Populares por Estructura

-   **Arrays/Strings:** Ventana deslizante, dos punteros, hashing.
-   **Linked Lists:** Inserción, eliminación, inversión, detección de ciclos.
-   **Stacks/Queues:** Análisis de expresiones, funciones de deshacer/rehacer, BFS.
-   **Trees/Graphs:** Recorridos (DFS, BFS), búsqueda del camino más corto (Dijkstra, A*).

---

## ✨ Guía Rápida para Resolver Problemas

Cuando te enfrentes a un nuevo problema, pregúntate: **"¿De qué se trata fundamentalmente este problema?"**

-   ¿Es sobre **orden** o **búsqueda** en datos ordenados? → **Binary Search**.
-   ¿Necesito **jerarquía** o relaciones padre-hijo? → **Trees (BST, Heap, Trie)**.
-   ¿Se trata de **optimización** encontrando la mejor solución entre muchas? → **Programación Dinámica**.
-   ¿El problema involucra **conexiones**, **redes** o **caminos**? → **Graphs (BFS, DFS)**.
-   ¿Necesito tomar decisiones óptimas en cada paso? → **Algoritmos Voraces (Greedy)**.
-   ¿Requiero búsquedas, inserciones y eliminaciones rápidas por clave? → **Hash Maps**.

Esta simple pregunta te guiará hacia la estructura de datos y el algoritmo más adecuados para una solución eficiente.

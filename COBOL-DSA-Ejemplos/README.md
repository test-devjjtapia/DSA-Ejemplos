# Proyecto de Estructuras de Datos y Algoritmos en COBOL

Este proyecto es un "fork" del repositorio original de ejemplos de Estructuras de Datos y Algoritmos (DSA) en Python, pero reimplementado en COBOL. El objetivo es demostrar cómo se pueden abordar y simular conceptos de DSA en un lenguaje como COBOL, que tiene un paradigma de programación y características muy diferentes a los lenguajes modernos como Python.

## Desafíos de Implementación en COBOL

La traducción de Python a COBOL presenta varios desafíos clave:

-   **Tipado Estático y Declaración Explícita:** COBOL es un lenguaje fuertemente tipado y requiere la declaración explícita de todas las variables y estructuras de datos con tamaños fijos. Esto contrasta con la flexibilidad de Python, donde las listas pueden crecer dinámicamente y los tipos de datos son inferidos.
-   **Ausencia de Punteros Nativos y Asignación Dinámica de Memoria:** COBOL no maneja punteros de la misma manera que lenguajes como C/C++ o Python (a través de referencias a objetos). La simulación de estructuras como listas enlazadas o árboles requiere el uso de índices de array y la gestión manual de la "memoria" dentro de arrays predefinidos.
-   **Simulación de Recursión:** COBOL no soporta la recursión de forma nativa con una pila de llamadas automática como Python. Para implementar algoritmos recursivos (como Merge Sort, Quick Sort o Fibonacci recursivo), es necesario simular la pila de llamadas manualmente utilizando estructuras de datos de pila (arrays) para almacenar el estado de cada "llamada" recursiva.
-   **Manejo de Errores y Excepciones:** COBOL utiliza códigos de retorno y verificaciones explícitas para el manejo de errores, a diferencia de los mecanismos de excepciones `try-except` de Python.
-   **Sintaxis Verbosa:** La sintaxis de COBOL es considerablemente más verbosa, lo que resulta en programas más largos para funcionalidades equivalentes.

## Estructura del Proyecto

La estructura del proyecto COBOL replica la del proyecto original en Python para facilitar la comparación y comprensión:

```
COBOL-DSA-Ejemplos/
├── README.md
├── data_structures/
│   ├── array.cbl
│   ├── queue.cbl
│   ├── stack.cbl
│   ├── linked_list.cbl
│   └── tree.cbl
└── algorithms/
    ├── searching/
    │   ├── linear_search.cbl
    │   └── binary_search.cbl
    ├── sorting/
    │   ├── bubble_sort.cbl
    │   ├── insertion_sort.cbl
    │   ├── merge_sort.cbl
    │   └── quick_sort.cbl
    └── recursion/
        ├── fibonacci.cbl
        └── list_reversal.cbl
```

---

## Requisitos y Compilación

Este proyecto está diseñado para ser compilado y ejecutado con **GnuCOBOL**, una implementación libre y de código abierto del lenguaje COBOL. GnuCOBOL es compatible con los estándares COBOL 85, COBOL 2002 y COBOL 2014.

### Instalación de GnuCOBOL

Las instrucciones de instalación varían según el sistema operativo:

#### Linux (Debian/Ubuntu)

```bash
sudo apt update
sudo apt install gnucobol
```

#### Linux (Fedora/RHEL/CentOS)

```bash
sudo dnf install gnucobol
```

#### Windows

1.  **Descargar:** Visita la página de GnuCOBOL en SourceForge: [https://sourceforge.net/projects/gnucobol/files/gnucobol/](https://sourceforge.net/projects/gnucobol/files/gnucobol/)
2.  Busca la última versión estable (por ejemplo, `gnucobol-x.x.x-mingw.zip` para Windows).
3.  **Extraer:** Descomprime el archivo ZIP en una ubicación de tu elección (por ejemplo, `C:\GnuCOBOL`).
4.  **Configurar PATH:** Añade el directorio `bin` de GnuCOBOL a la variable de entorno `PATH` de tu sistema para poder ejecutar el compilador desde cualquier ubicación en la línea de comandos.

#### macOS

La forma más sencilla de instalar GnuCOBOL en macOS es a través de Homebrew:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install gnucobol
```

### Compilación y Ejecución

Una vez que GnuCOBOL esté instalado, puedes compilar y ejecutar los programas COBOL de este proyecto. Navega hasta el directorio donde se encuentra el archivo `.cbl` que deseas compilar y utiliza los siguientes comandos:

```bash
cobc -x -free -o mi_programa mi_programa.cbl
./mi_programa
```

-   `cobc`: Es el comando del compilador GnuCOBOL.
-   `-x`: Indica que se debe crear un ejecutable.
-   `-free`: Permite el formato libre del código fuente (recomendado para este proyecto).
-   `-o mi_programa`: Especifica el nombre del archivo de salida ejecutable.
-   `mi_programa.cbl`: Es el archivo fuente COBOL a compilar.

---

## 🧠 Estructuras de Datos Implementadas

Cada estructura de datos se ha adaptado para funcionar dentro de las limitaciones de COBOL, utilizando arrays de tamaño fijo y lógica de punteros basada en índices.

| Estructura | Descripción | Notas de Implementación en COBOL |
| :--- | :--- | :--- |
| **Array (Arreglo)** | Colección secuencial de elementos. | Implementado con `OCCURS` (arrays de tamaño fijo). Las operaciones dinámicas de Python (`append`, `insert`, `pop`, `remove`) se simulan con movimientos de datos y gestión manual del tamaño lógico del array. |
| **Queue (Cola)** | Estructura FIFO (First-In, First-Out). | Utiliza un array (`OCCURS`) y dos índices (`FRONT-POINTER`, `REAR-POINTER`) para gestionar la entrada y salida de elementos. |
| **Stack (Pila)** | Estructura LIFO (Last-In, First-Out). | Implementada con un array (`OCCURS`) y un índice (`TOP-POINTER`) que apunta al último elemento añadido. |
| **Linked List (Lista Enlazada)** | Nodos conectados por referencias. | Simulación compleja: se utiliza un array de `NODES` donde cada `NODE` contiene el dato, un índice al `NEXT` nodo y un flag `ACTIVE`. La gestión de nodos libres y la asignación se realiza manualmente. |
| **Tree (Árbol de Búsqueda Binaria)** | Estructura jerárquica con nodos y subárboles. | Simulación compleja: se usa un array de `NODES`, donde cada `NODE` tiene el valor, y dos índices (`LEFT-CHILD-POINTER`, `RIGHT-CHILD-POINTER`) para sus hijos. La inserción y búsqueda se realizan iterativamente. |

---

## ⚙️ Algoritmos Implementados

Los algoritmos se han adaptado para operar sobre las estructuras de datos COBOL, a menudo requiriendo un enfoque iterativo o la simulación manual de la recursión.

### Búsqueda (Searching)

| Algoritmo | Descripción | Notas de Implementación en COBOL |
| :--- | :--- | :--- |
| **Linear Search** | Recorre la colección elemento por elemento. | Implementado con un bucle `PERFORM VARYING` simple. |
| **Binary Search** | Búsqueda eficiente en colecciones **ordenadas**. | Utiliza un bucle `PERFORM UNTIL` y manipulación de índices (`LOW-INDEX`, `HIGH-INDEX`, `MID-INDEX`) para reducir el espacio de búsqueda. |

### Ordenamiento (Sorting)

| Algoritmo | Descripción | Notas de Implementación en COBOL |
| :--- | :--- | :--- |
| **Bubble Sort** | Compara e intercambia elementos adyacentes. | Implementado con bucles `PERFORM VARYING` anidados para las pasadas y comparaciones. |
| **Selection Sort** | Encuentra el mínimo y lo coloca en su posición. | Utiliza bucles `PERFORM VARYING` anidados para encontrar el elemento mínimo y realizar el intercambio. |
| **Insertion Sort** | Construye la lista ordenada insertando elementos. | Implementado con bucles `PERFORM` para desplazar elementos y encontrar la posición correcta. |
| **Merge Sort** | Algoritmo "Divide y Vencerás" que fusiona sub-listas. | Implementación iterativa para evitar la complejidad de la recursión directa en COBOL. Utiliza un enfoque de "bottom-up" fusionando sub-arrays de tamaño creciente. |
| **Quick Sort** | Algoritmo "Divide y Vencerás" que particiona la lista. | Implementación iterativa utilizando una pila (`STACK-AREA`) para gestionar los rangos de sub-arrays a ordenar, simulando las llamadas recursivas. |

### Recursión

La recursión se simula en COBOL utilizando una pila explícita para almacenar el contexto de cada "llamada" recursiva (parámetros, direcciones de retorno, etc.).

| Concepto | Descripción | Notas de Implementación en COBOL |
| :--- | :--- | :--- |
| **Fibonacci** | Cálculo de la secuencia de Fibonacci. | Se proporciona una versión iterativa (eficiente) y una versión que simula la recursión utilizando una pila para gestionar las llamadas y los resultados intermedios. |
| **Inversión de Lista** | Invierte una lista. | Simula la recursión utilizando una pila para almacenar los elementos de la lista y luego los extrae en orden inverso. |

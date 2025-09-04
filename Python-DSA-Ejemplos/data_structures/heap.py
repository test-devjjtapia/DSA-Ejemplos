"""
Demostración de Heaps (Montículos) en Python.

Un Heap es una estructura de datos de árbol binario que satisface la "propiedad del heap":
- En un Min-Heap, para cualquier nodo dado C, si P es un nodo padre de C, entonces el valor
  de P es menor o igual al valor de C. (La raíz es el elemento más pequeño).
- En un Max-Heap, el valor de P es mayor or igual al valor de C.

Python proporciona el módulo `heapq`, que implementa un Min-Heap.
Los heaps son ideales para implementar colas de prioridad.
"""

import heapq

# --- 1. Crear un Heap ---
print("--- Crear un Heap ---")

# `heapq` trabaja sobre listas de Python.
# Un heap es simplemente una lista que mantiene la propiedad del heap.
cola_prioridad = []

# Añadir elementos al heap (cola de prioridad)
# heappush mantiene la propiedad del heap.
heapq.heappush(cola_prioridad, 30) # Tarea con prioridad 30
heapq.heappush(cola_prioridad, 10) # Tarea con prioridad 10
heapq.heappush(cola_prioridad, 20) # Tarea con prioridad 20
heapq.heappush(cola_prioridad, 5)  # Tarea con prioridad 5

print(f"Heap (lista interna): {cola_prioridad}")
# Nota: La lista no parece ordenada, pero la propiedad del heap se mantiene.
# El elemento más pequeño (cola_prioridad[0]) siempre está en la raíz.

# --- 2. Extraer el Elemento de Menor Prioridad ---
print("\n--- Extraer el Elemento de Menor Prioridad ---")

# heappop() elimina y devuelve el elemento más pequeño del heap.

print("Extrayendo elementos en orden de prioridad (del más pequeño al más grande):")
while cola_prioridad:
    elemento_menor = heapq.heappop(cola_prioridad)
    print(f"Elemento extraído: {elemento_menor}")
    print(f"  Heap restante: {cola_prioridad}")

# --- 3. Convertir una Lista en un Heap (heapify) ---
print("\n--- Convertir una Lista en un Heap ---")

lista_desordenada = [9, 5, 2, 8, 1, 7]
print(f"Lista original desordenada: {lista_desordenada}")

# heapify() transforma la lista en un heap in-place (en tiempo O(n))
heapq.heapify(lista_desordenada)
print(f"Lista después de heapify: {lista_desordenada}")

print("\nExtrayendo elementos del heap creado con heapify:")
while lista_desordenada:
    print(f"Elemento extraído: {heapq.heappop(lista_desordenada)}")

# --- 4. Acceder a los elementos más pequeños ---
print("\n--- Acceder a los elementos más pequeños ---")

# nsmallest() devuelve los N elementos más pequeños sin modificar el heap
heap_grande = [10, 2, 9, 4, 5, 1, 7, 8, 3, 6]
heapq.heapify(heap_grande)

print(f"Heap: {heap_grande}")
print(f"Los 3 elementos más pequeños: {heapq.nsmallest(3, heap_grande)}")
print(f"El heap no ha cambiado: {heap_grande}")

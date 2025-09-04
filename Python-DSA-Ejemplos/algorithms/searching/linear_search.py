"""
Implementación del algoritmo de Búsqueda Lineal (Linear Search).

La búsqueda lineal es el método de búsqueda más simple. Consiste en recorrer
una colección (como una lista o arreglo) de principio a fin, elemento por elemento,
hasta encontrar el objetivo o llegar al final.

Es simple pero puede ser ineficiente para colecciones grandes.

Complejidad de tiempo: O(n)
"""

def linear_search(arr, target):
    """
    Realiza una búsqueda lineal en una lista.

    Args:
        arr: La lista de elementos.
        target: El elemento a buscar.

    Returns:
        El índice del elemento si se encuentra, de lo contrario -1.
    """
    for i in range(len(arr)):
        if arr[i] == target:
            return i # Se encontró el elemento, se devuelve el índice
    return -1 # El elemento no fue encontrado después de recorrer toda la lista

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    mi_lista = [10, 50, 30, 70, 80, 20, 90, 40]
    
    print(f"Lista: {mi_lista}")

    # --- Búsqueda de un elemento que existe ---
    objetivo1 = 80
    indice1 = linear_search(mi_lista, objetivo1)
    if indice1 != -1:
        print(f"El elemento {objetivo1} se encuentra en el índice: {indice1}")
    else:
        print(f"El elemento {objetivo1} no se encontró en la lista.")

    # --- Búsqueda de un elemento que NO existe ---
    objetivo2 = 100
    indice2 = linear_search(mi_lista, objetivo2)
    if indice2 != -1:
        print(f"El elemento {objetivo2} se encuentra en el índice: {indice2}")
    else:
        print(f"El elemento {objetivo2} no se encontró en la lista.")

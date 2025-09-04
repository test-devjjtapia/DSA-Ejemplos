"""
Implementación del algoritmo de Búsqueda Binaria (Binary Search).

La búsqueda binaria es un algoritmo eficiente para encontrar un elemento en una
lista **ordenada**. Funciona dividiendo repetidamente a la mitad la porción
de la lista que podría contener al elemento, hasta reducir las ubicaciones
posibles a solo una.

Requisito: La lista debe estar ordenada.
Complejidad de tiempo: O(log n)
"""

def binary_search(sorted_list, target):
    """
    Realiza una búsqueda binaria en una lista ordenada.

    Args:
        sorted_list: Una lista de elementos ordenados.
        target: El elemento a buscar.

    Returns:
        El índice del elemento si se encuentra, de lo contrario -1.
    """
    low = 0
    high = len(sorted_list) - 1

    while low <= high:
        mid = (low + high) // 2
        guess = sorted_list[mid]

        if guess == target:
            return mid
        if guess > target:
            high = mid - 1
        else:
            low = mid + 1
            
    return -1 # El elemento no fue encontrado

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    # La lista DEBE estar ordenada
    mi_lista_ordenada = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91]
    
    print(f"Lista ordenada: {mi_lista_ordenada}")

    # --- Búsqueda de un elemento que existe ---
    objetivo1 = 23
    indice1 = binary_search(mi_lista_ordenada, objetivo1)
    if indice1 != -1:
        print(f"El elemento {objetivo1} se encuentra en el índice: {indice1}")
    else:
        print(f"El elemento {objetivo1} no se encontró en la lista.")

    # --- Búsqueda de un elemento que NO existe ---
    objetivo2 = 40
    indice2 = binary_search(mi_lista_ordenada, objetivo2)
    if indice2 != -1:
        print(f"El elemento {objetivo2} se encuentra en el índice: {indice2}")
    else:
        print(f"El elemento {objetivo2} no se encontró en la lista.")

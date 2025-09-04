"""
Implementación del algoritmo de Ordenamiento por Selección (Selection Sort).

Selection Sort es un algoritmo de ordenamiento simple que divide la lista en dos partes: una sublista ordenada que se construye de izquierda a derecha y una sublista de los elementos restantes por ordenar.

El algoritmo procede encontrando el elemento más pequeño en la sublista no ordenada y lo intercambia con el elemento más a la izquierda de la sublista no ordenada.

Complejidad de tiempo: O(n^2)
"""

def selection_sort(arr):
    """
    Ordena una lista utilizando el método de selección.

    Args:
        arr: La lista de elementos a ordenar.

    Returns:
        Una nueva lista con los elementos ordenados.
    """
    n = len(arr)
    sorted_arr = arr[:]

    # Recorrer toda la lista
    for i in range(n):
        # Encontrar el mínimo en la lista no ordenada
        min_idx = i
        for j in range(i + 1, n):
            if sorted_arr[j] < sorted_arr[min_idx]:
                min_idx = j
        
        # Intercambiar el elemento mínimo encontrado con el primer elemento
        sorted_arr[i], sorted_arr[min_idx] = sorted_arr[min_idx], sorted_arr[i]

    return sorted_arr

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    mi_lista_desordenada = [64, 25, 12, 22, 11]
    
    print(f"Lista desordenada: {mi_lista_desordenada}")
    
    lista_ordenada = selection_sort(mi_lista_desordenada)
    
    print(f"Lista ordenada:    {lista_ordenada}")

    # Ejemplo con otra lista
    otra_lista = [38, 27, 43, 3, 9, 82, 10]
    print(f"\nLista desordenada: {otra_lista}")
    lista_ordenada_2 = selection_sort(otra_lista)
    print(f"Lista ordenada:    {lista_ordenada_2}")

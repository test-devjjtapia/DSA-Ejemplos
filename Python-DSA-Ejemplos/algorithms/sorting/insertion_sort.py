"""
Implementación del algoritmo de Ordenamiento por Inserción (Insertion Sort).

Insertion Sort es un algoritmo de ordenamiento simple que construye la lista ordenada
final un elemento a la vez. Es mucho menos eficiente en listas grandes que algoritmos
más avanzados como quicksort, heapsort o merge sort.

Funciona tomando cada elemento de la lista de entrada y lo inserta en su posición
correcta en la lista ya ordenada.

Complejidad de tiempo: O(n^2)
"""

def insertion_sort(arr):
    """
    Ordena una lista utilizando el método de inserción.

    Args:
        arr: La lista de elementos a ordenar.

    Returns:
        Una nueva lista con los elementos ordenados.
    """
    sorted_arr = arr[:]

    # Recorremos desde el segundo elemento
    for i in range(1, len(sorted_arr)):
        key = sorted_arr[i] # El elemento que vamos a insertar
        
        # Mover los elementos de sorted_arr[0..i-1] que son mayores que key
        # a una posición adelante de su posición actual
        j = i - 1
        while j >= 0 and key < sorted_arr[j]:
            sorted_arr[j + 1] = sorted_arr[j]
            j -= 1
        sorted_arr[j + 1] = key

    return sorted_arr

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    mi_lista_desordenada = [12, 11, 13, 5, 6]
    
    print(f"Lista desordenada: {mi_lista_desordenada}")
    
    lista_ordenada = insertion_sort(mi_lista_desordenada)
    
    print(f"Lista ordenada:    {lista_ordenada}")

    # Ejemplo con otra lista
    otra_lista = [64, 34, 25, 12, 22, 11, 90]
    print(f"\nLista desordenada: {otra_lista}")
    lista_ordenada_2 = insertion_sort(otra_lista)
    print(f"Lista ordenada:    {lista_ordenada_2}")

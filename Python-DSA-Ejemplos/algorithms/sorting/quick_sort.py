"""

Implementación del algoritmo de Ordenamiento Rápido (Quick Sort).

Quick Sort es un algoritmo de ordenamiento eficiente que sigue el paradigma de
"Divide y Vencerás". Funciona seleccionando un elemento 'pivote' y particionando
los otros elementos en dos sub-arreglos, según si son menores o mayores que el pivote.
Luego, los sub-arreglos se ordenan recursivamente.

Complejidad de tiempo promedio: O(n log n)
"""

def quick_sort(arr):
    """
    Ordena una lista utilizando el método Quick Sort.

    Args:
        arr: La lista de elementos a ordenar.

    Returns:
        Una nueva lista con los elementos ordenados.
    """
    if len(arr) <= 1:
        return arr
    else:
        # Tomamos el último elemento como pivote
        pivot = arr[-1]
        
        # Elementos menores que el pivote
        less = [x for x in arr[:-1] if x <= pivot]
        
        # Elementos mayores que el pivote
        greater = [x for x in arr[:-1] if x > pivot]
        
        return quick_sort(less) + [pivot] + quick_sort(greater)

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    mi_lista_desordenada = [10, 7, 8, 9, 1, 5]
    
    print(f"Lista desordenada: {mi_lista_desordenada}")
    
    lista_ordenada = quick_sort(mi_lista_desordenada)
    
    print(f"Lista ordenada:    {lista_ordenada}")

    # Ejemplo con otra lista
    otra_lista = [64, 34, 25, 12, 22, 11, 90]
    print(f"\nLista desordenada: {otra_lista}")
    lista_ordenada_2 = quick_sort(otra_lista)
    print(f"Lista ordenada:    {lista_ordenada_2}")

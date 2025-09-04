"""
Implementación del algoritmo de Ordenamiento por Fusión (Merge Sort).

Merge Sort es un algoritmo eficiente, de propósito general, basado en la comparación
que sigue el paradigma de "Divide y Vencerás". Conceptualmente, funciona de la siguiente manera:

1. Divide la lista no ordenada en n sub-listas, cada una conteniendo un elemento.
2. Fusiona repetidamente las sub-listas para producir nuevas sub-listas ordenadas hasta que solo quede una.

Complejidad de tiempo: O(n log n)
"""

def merge_sort(arr):
    """
    Ordena una lista utilizando el método Merge Sort.

    Args:
        arr: La lista de elementos a ordenar.

    Returns:
        Una nueva lista con los elementos ordenados.
    """
    if len(arr) > 1:
        mid = len(arr) // 2  # Encontrar el medio de la lista
        L = arr[:mid]       # Dividir los elementos en dos mitades
        R = arr[mid:]

        merge_sort(L)  # Ordenar la primera mitad
        merge_sort(R)  # Ordenar la segunda mitad

        i = j = k = 0

        # Copiar datos a los arreglos temporales L[] y R[]
        while i < len(L) and j < len(R):
            if L[i] < R[j]:
                arr[k] = L[i]
                i += 1
            else:
                arr[k] = R[j]
                j += 1
            k += 1

        # Comprobar si quedó algún elemento
        while i < len(L):
            arr[k] = L[i]
            i += 1
            k += 1

        while j < len(R):
            arr[k] = R[j]
            j += 1
            k += 1
    return arr

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    mi_lista_desordenada = [38, 27, 43, 3, 9, 82, 10]
    
    print(f"Lista desordenada: {mi_lista_desordenada}")
    
    # Merge sort ordena la lista in-place, pero la retornamos por consistencia
    lista_ordenada = merge_sort(mi_lista_desordenada[:]) # Pasamos una copia
    
    print(f"Lista ordenada:    {lista_ordenada}")

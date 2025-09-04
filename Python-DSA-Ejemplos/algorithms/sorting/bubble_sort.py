"""
Implementación del algoritmo de Ordenamiento de Burbuja (Bubble Sort).

Bubble Sort es un algoritmo de ordenamiento simple que recorre repetidamente la lista,
compara elementos adyacentes y los intercambia si están en el orden incorrecto.
El paso a través de la lista se repite hasta que la lista esté ordenada.

Aunque es simple de entender, es ineficiente para listas grandes.

Complejidad de tiempo: O(n^2)
"""

def bubble_sort(arr):
    """
    Ordena una lista utilizando el método de la burbuja.

    Args:
        arr: La lista de elementos a ordenar.

    Returns:
        Una nueva lista con los elementos ordenados.
    """
    n = len(arr)
    # Creamos una copia para no modificar la lista original
    sorted_arr = arr[:]

    # Recorremos todos los elementos del arreglo
    for i in range(n):
        # Los últimos i elementos ya están en su lugar
        for j in range(0, n - i - 1):
            # Recorremos el arreglo de 0 a n-i-1
            # Intercambiamos si el elemento encontrado es mayor que el siguiente
            if sorted_arr[j] > sorted_arr[j + 1]:
                sorted_arr[j], sorted_arr[j + 1] = sorted_arr[j + 1], sorted_arr[j]
    
    return sorted_arr

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    mi_lista_desordenada = [64, 34, 25, 12, 22, 11, 90]
    
    print(f"Lista desordenada: {mi_lista_desordenada}")
    
    lista_ordenada = bubble_sort(mi_lista_desordenada)
    
    print(f"Lista ordenada:    {lista_ordenada}")

    # Ejemplo con otra lista
    otra_lista = [5, 1, 4, 2, 8]
    print(f"\nLista desordenada: {otra_lista}")
    lista_ordenada_2 = bubble_sort(otra_lista)
    print(f"Lista ordenada:    {lista_ordenada_2}")

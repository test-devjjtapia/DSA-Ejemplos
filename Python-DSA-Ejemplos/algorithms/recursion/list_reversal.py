"""
Ejemplo de c3mo invertir una lista utilizando recursi3n.

Este es un ejemplo cl3sico para demostrar c3mo se puede pensar de manera recursiva
para resolver un problema.
"""

def reverse_list_recursive(lst):
    """
    Invierte una lista utilizando un enfoque recursivo.

    Args:
        lst: La lista a invertir.

    Returns:
        Una nueva lista invertida.
    """
    # Caso base: si la lista est3 vac3a o tiene un solo elemento, ya est3 invertida.
    if len(lst) <= 1:
        return lst
    
    # Paso recursivo: 
    # 1. Toma el primer elemento (lst[0])
    # 2. Llama recursivamente a la funci3n con el resto de la lista (lst[1:])
    # 3. Concatena el primer elemento al final de la lista invertida devuelta.
    return reverse_list_recursive(lst[1:]) + [lst[0]]

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    mi_lista = [1, 2, 3, 4, 5]
    print(f"Lista original: {mi_lista}")
    
    lista_invertida = reverse_list_recursive(mi_lista)
    print(f"Lista invertida: {lista_invertida}")

    # Ejemplo con otra lista
    otra_lista = ["a", "b", "c", "d"]
    print(f"\nLista original: {otra_lista}")
    lista_invertida_2 = reverse_list_recursive(otra_lista)
    print(f"Lista invertida: {lista_invertida_2}")

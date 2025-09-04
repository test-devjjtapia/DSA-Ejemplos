"""

Demostración de Arrays (Arreglos) en Python.

En Python, no existe un tipo de dato 'array' nativo como en otros lenguajes (ej. C++ o Java).
En su lugar, las 'listas' (`list`) de Python sirven como arreglos dinámicos.

Un arreglo es una colección de elementos almacenados en ubicaciones de memoria contiguas,
accesibles mediante un índice.

"""

# --- 1. Creación y Acceso ---
print("--- Creación y Acceso ---")
# Las listas de Python son arreglos dinámicos
frutas = ["manzana", "banana", "cereza", "dátil"]
print(f"Lista completa: {frutas}")

# Acceso por índice (basado en 0)
print(f"Primer elemento (índice 0): {frutas[0]}")
print(f"Tercer elemento (índice 2): {frutas[2]}")
print(f"Último elemento (índice -1): {frutas[-1]}")

# --- 2. Modificación ---
print("\n--- Modificación ---")
frutas[1] = "arándano"
print(f"Lista después de modificar el índice 1: {frutas}")

# --- 3. Añadir elementos ---
print("\n--- Añadir elementos ---")
# Añadir al final
frutas.append("frambuesa")
print(f"Después de append('frambuesa'): {frutas}")

# Insertar en una posición específica
frutas.insert(2, "kiwi") # Inserta 'kiwi' en el índice 2
print(f"Después de insert(2, 'kiwi'): {frutas}")

# --- 4. Eliminar elementos ---
print("\n--- Eliminar elementos ---")
# Eliminar el último elemento
elemento_eliminado = frutas.pop()
print(f"Elemento eliminado con pop(): {elemento_eliminado}")
print(f"Lista actual: {frutas}")

# Eliminar por índice
elemento_eliminado_indice = frutas.pop(1) # Elimina el elemento en el índice 1
print(f"Elemento eliminado con pop(1): {elemento_eliminado_indice}")
print(f"Lista actual: {frutas}")

# Eliminar por valor (la primera ocurrencia)
frutas.remove("cereza")
print(f"Después de remove('cereza'): {frutas}")

# --- 5. Slicing (Rebanado) ---
print("\n--- Slicing ---")
numeros = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
print(f"Lista de números: {numeros}")

# Obtener un sub-arreglo (del índice 2 al 4)
sub_lista = numeros[2:5]
print(f"Sub-lista de [2:5]: {sub_lista}")

# Desde el inicio hasta el índice 3
desde_inicio = numeros[:4]
print(f"Sub-lista de [:4]: {desde_inicio}")

# Desde el índice 5 hasta el final
hasta_final = numeros[5:]
print(f"Sub-lista de [5:]: {hasta_final}")

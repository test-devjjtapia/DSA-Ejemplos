"""
Demostración de Matrices (Arrays 2D) en Python.

Una matriz es una colección de datos bidimensional, organizada en filas y columnas.
En Python, las matrices se pueden implementar fácilmente usando listas de listas,
donde cada lista interna representa una fila.
"""

# --- 1. Creación de una Matriz ---
print("---\nCreación de una Matriz ---")

# Matriz 3x4 (3 filas, 4 columnas)
matriz = [
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12]
]

print("Matriz creada:")
for fila in matriz:
    print(fila)

# --- 2. Acceso a Elementos ---
print("\n--- Acceso a Elementos ---")

# Para acceder a un elemento, se usa la sintaxis matriz[fila][columna]
# (los índices empiezan en 0)

# Elemento en la fila 0, columna 2
elemento_0_2 = matriz[0][2]
print(f"Elemento en la fila 0, columna 2: {elemento_0_2}") # Debería ser 3

# Elemento en la fila 2, columna 3
elemento_2_3 = matriz[2][3]
print(f"Elemento en la fila 2, columna 3: {elemento_2_3}") # Debería ser 12

# --- 3. Modificación de Elementos ---
print("\n--- Modificación de Elementos ---")

print(f"Valor original en [1][1]: {matriz[1][1]}")
matriz[1][1] = 99 # Cambiamos el 6 por 99
print(f"Nuevo valor en [1][1]: {matriz[1][1]}")

print("\nMatriz después de la modificación:")
for fila in matriz:
    print(fila)

# --- 4. Recorrer la Matriz ---
print("\n--- Recorrer la Matriz ---")

num_filas = len(matriz)
num_columnas = len(matriz[0]) # Asumimos que todas las filas tienen la misma longitud

print("Recorriendo cada elemento:")
for i in range(num_filas):
    for j in range(num_columnas):
        print(f"Elemento en [{i}][{j}]: {matriz[i][j]}")

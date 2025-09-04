"""
Demostración de Hash Maps (Tablas Hash) en Python.

En Python, la estructura de datos de Tabla Hash se implementa a través del tipo de dato
`dict` (diccionario). Un diccionario almacena datos en pares clave-valor.

Los diccionarios están altamente optimizados para recuperar valores cuando se conoce la clave.
La clave debe ser de un tipo de dato inmutable (como strings, números o tuplas).

"""

# --- 1. Creación de un Diccionario ---
print("---""Creación de un Diccionario""---")

# Crear un diccionario vacío
diccionario_vacio = {}

# Crear un diccionario con pares clave-valor
capitales = {
    "España": "Madrid",
    "Francia": "París",
    "Italia": "Roma"
}
print(f"Diccionario de capitales: {capitales}")

# --- 2. Acceso a Valores ---
print("\n---""Acceso a Valores""---")

# Acceder a un valor usando su clave
captal_francia = capitales["Francia"]
print(f"La capital de Francia es: {capital_francia}")

# Usar el método .get() es más seguro, ya que no da error si la clave no existe
captal_alemania = capitales.get("Alemania")
print(f"La capital de Alemania es: {capital_alemania}") # Devuelve None

captal_alemania_default = capitales.get("Alemania", "No encontrada")
print(f"La capital de Alemania es: {capital_alemania_default}") # Devuelve 'No encontrada'

# --- 3. Añadir o Modificar Pares ---
print("\n---""Añadir o Modificar Pares""---")

# Añadir un nuevo par clave-valor
captales["Alemania"] = "Berlín"
print(f"Diccionario después de añadir Alemania: {capitales}")

# Modificar un valor existente
captales["España"] = "Madrid, la capital"
print(f"Diccionario después de modificar España: {capitales}")

# --- 4. Eliminar Pares ---
print("\n---""Eliminar Pares""---")

# Eliminar un par usando 'del'
del capitales["Italia"]
print(f"Después de 'del capitales["Italia"]': {capitales}")

# Eliminar usando .pop(), que devuelve el valor eliminado
valor_eliminado = capitales.pop("Francia")
print(f"Valor eliminado con pop('Francia'): {valor_eliminado}")
print(f"Diccionario actual: {capitales}")

# --- 5. Iterar sobre un Diccionario ---
print("\n---""Iterar sobre un Diccionario""---")

captales["Japón"] = "Tokio"
captales["Portugal"] = "Lisboa"

print("\nIterando sobre las claves:")
for pais in capitales.keys():
    print(pais)

print("\nIterando sobre los valores:")
for capital in capitales.values():
    print(capital)

print("\nIterando sobre los pares clave-valor (.items()):")
for pais, capital in capitales.items():
    print(f"La capital de {pais} es {capital}")

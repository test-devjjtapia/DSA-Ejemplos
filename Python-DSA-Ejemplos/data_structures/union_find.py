"""
Implementación de Union-Find (o Disjoint Set Union - DSU).

Union-Find es una estructura de datos que realiza un seguimiento de un conjunto de
elementos particionados en una serie de conjuntos disjuntos (no superpuestos).

Proporciona dos operaciones principales:
1. `find(i)`: Determina el representante (o raíz) del conjunto al que pertenece el elemento `i`.
2. `union(i, j)`: Fusiona los dos conjuntos que contienen a `i` y `j`.

Se utiliza comúnmente en algoritmos de grafos, como el de Kruskal para encontrar
árboles de expansión mínima, o para detectar ciclos en un grafo.
"""

class UnionFind:
    """Implementación de Union-Find con optimizaciones (unión por rango y compresión de caminos)."""
    def __init__(self, size):
        # Inicialmente, cada elemento es su propio padre (n conjuntos separados)
        self.parent = list(range(size))
        # El rango (o altura) de cada conjunto, inicialmente 1
        self.rank = [1] * size

    def find(self, i):
        """Encuentra el representante del conjunto de i con compresión de caminos."""
        if self.parent[i] == i:
            return i
        # Compresión de caminos: hacer que el padre de i sea la raíz
        self.parent[i] = self.find(self.parent[i])
        return self.parent[i]

    def union(self, i, j):
        """Une los conjuntos que contienen a i y j usando unión por rango."""
        root_i = self.find(i)
        root_j = self.find(j)

        if root_i != root_j:
            # Unión por rango: adjuntar el árbol más pequeño al más grande
            if self.rank[root_i] > self.rank[root_j]:
                self.parent[root_j] = root_i
            elif self.rank[root_i] < self.rank[root_j]:
                self.parent[root_i] = root_j
            else:
                self.parent[root_j] = root_i
                self.rank[root_i] += 1
            return True # Se realizó la unión
        return False # Ya estaban en el mismo conjunto

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    # Creamos una estructura para 10 elementos (0 a 9)
    uf = UnionFind(10)

    print(f"Padres iniciales: {uf.parent}")

    print("\n--- Realizando uniones ---")
    uf.union(1, 2)
    print("union(1, 2)")
    uf.union(2, 3)
    print("union(2, 3)")
    uf.union(4, 5)
    print("union(4, 5)")
    uf.union(6, 7)
    print("union(6, 7)")
    uf.union(5, 6)
    print("union(5, 6)")
    uf.union(3, 7)
    print("union(3, 7)")

    print(f"\nPadres después de las uniones: {uf.parent}")

    print("\n--- Comprobando conjuntos ---")
    # find() nos da el representante de cada conjunto
    print(f"Representante de 1: {uf.find(1)}")
    print(f"Representante de 5: {uf.find(5)}")
    print(f"Representante de 9: {uf.find(9)}")

    # Comprobar si dos elementos están en el mismo conjunto
    print(f"\n¿Están 1 y 7 en el mismo conjunto? {uf.find(1) == uf.find(7)}") # True
    print(f"¿Están 4 y 8 en el mismo conjunto? {uf.find(4) == uf.find(8)}") # False

"""
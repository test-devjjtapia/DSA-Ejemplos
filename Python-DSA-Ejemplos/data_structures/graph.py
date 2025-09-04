"""
Implementación de un Grafo (Graph).

Un grafo es una estructura de datos que consiste en un conjunto de vértices (o nodos)
y un conjunto de aristas (o conexiones) que conectan pares de vértices.

Esta implementación utiliza una lista de adyacencia, donde cada vértice tiene una
lista de los vértices a los que está conectado.
"""
from collections import deque

class Graph:
    """Clase para representar un grafo usando una lista de adyacencia."""
    def __init__(self):
        self.adj_list = {}

    def add_vertex(self, vertex):
        """Añade un vértice al grafo."""
        if vertex not in self.adj_list:
            self.adj_list[vertex] = []

    def add_edge(self, v1, v2, directed=False):
        """Añade una arista entre dos vértices."""
        if v1 in self.adj_list and v2 in self.adj_list:
            self.adj_list[v1].append(v2)
            if not directed:
                self.adj_list[v2].append(v1) # Para grafos no dirigidos
        else:
            print("Error: Uno o ambos vértices no existen.")

    def bfs(self, start_vertex):
        """Recorrido en Anchura (Breadth-First Search)."""
        if start_vertex not in self.adj_list:
            return []
        visited = {start_vertex}
        queue = deque([start_vertex])
        result = []
        while queue:
            vertex = queue.popleft()
            result.append(vertex)
            for neighbor in self.adj_list[vertex]:
                if neighbor not in visited:
                    visited.add(neighbor)
                    queue.append(neighbor)
        return result

    def dfs(self, start_vertex, visited=None):
        """Recorrido en Profundidad (Depth-First Search)."""
        if visited is None:
            visited = set()
        if start_vertex not in visited:
            visited.add(start_vertex)
            # Aquí podrías añadir el vértice a una lista de resultados
            print(start_vertex, end=' ')
            for neighbor in self.adj_list[start_vertex]:
                self.dfs(neighbor, visited)

    def display(self):
        """Muestra la lista de adyacencia del grafo."""
        for vertex, neighbors in self.adj_list.items():
            print(f"{vertex}: {', '.join(neighbors)}")

# --- Ejemplo de Uso ---
if __name__ == "__main__":
    g = Graph()
    vertices = ['A', 'B', 'C', 'D', 'E', 'F']
    for v in vertices:
        g.add_vertex(v)

    g.add_edge('A', 'B')
    g.add_edge('A', 'C')
    g.add_edge('B', 'D')
    g.add_edge('C', 'E')
    g.add_edge('D', 'E')
    g.add_edge('D', 'F')
    g.add_edge('E', 'F')

    print("--- Lista de Adyacencia del Grafo ---")
    g.display()

    print("\n--- Recorrido en Anchura (BFS) empezando desde 'A' ---")
    bfs_result = g.bfs('A')
    print(bfs_result)

    print("\n--- Recorrido en Profundidad (DFS) empezando desde 'A' ---")
    g.dfs('A')
    print()

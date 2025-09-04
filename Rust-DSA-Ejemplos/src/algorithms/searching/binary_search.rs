//! Módulo para demostrar el algoritmo de Búsqueda Binaria (Binary Search) en Rust.

// Realiza una búsqueda binaria en un slice ordenado de enteros.
// Retorna el índice del elemento si se encuentra, de lo contrario None.
pub fn binary_search(arr: &[i32], target: i32) -> Option<usize> {
    let mut low = 0;
    let mut high = arr.len() - 1;

    while low <= high {
        let mid = low + (high - low) / 2; // Evita desbordamiento potencial
        let guess = arr[mid];

        if guess == target {
            return Some(mid); // Elemento encontrado
        } else if guess > target {
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }
    None // Elemento no encontrado
}

// Función de demostración para la Búsqueda Binaria
pub fn binary_search_demo() {
    // La lista DEBE estar ordenada
    let mi_lista_ordenada = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91];

    println!("Lista ordenada: {:?}", mi_lista_ordenada);

    // --- Búsqueda de un elemento que existe ---
    let objetivo1 = 23;
    match binary_search(&mi_lista_ordenada, objetivo1) {
        Some(indice) => println!("El elemento {} se encuentra en el índice: {}", objetivo1, indice),
        None => println!("El elemento {} no se encontró en la lista.", objetivo1),
    }

    // --- Búsqueda de un elemento que NO existe ---
    let objetivo2 = 40;
    match binary_search(&mi_lista_ordenada, objetivo2) {
        Some(indice) => println!("El elemento {} se encuentra en el índice: {}", objetivo2, indice),
        None => println!("El elemento {} no se encontró en la lista.", objetivo2),
    }
}

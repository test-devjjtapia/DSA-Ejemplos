//! Módulo para demostrar el algoritmo de Búsqueda Lineal (Linear Search) en Rust.

// Realiza una búsqueda lineal en un slice de enteros.
// Retorna el índice del elemento si se encuentra, de lo contrario None.
pub fn linear_search(arr: &[i32], target: i32) -> Option<usize> {
    for (i, &item) in arr.iter().enumerate() {
        if item == target {
            return Some(i); // Se encontró el elemento, se devuelve el índice
        }
    }
    None // El elemento no fue encontrado
}

// Función de demostración para la Búsqueda Lineal
pub fn linear_search_demo() {
    let mi_lista = [10, 50, 30, 70, 80, 20, 90, 40];

    println!("Lista: {:?}", mi_lista);

    // --- Búsqueda de un elemento que existe ---
    let objetivo1 = 80;
    match linear_search(&mi_lista, objetivo1) {
        Some(indice) => println!("El elemento {} se encuentra en el índice: {}", objetivo1, indice),
        None => println!("El elemento {} no se encontró en la lista.", objetivo1),
    }

    // --- Búsqueda de un elemento que NO existe ---
    let objetivo2 = 100;
    match linear_search(&mi_lista, objetivo2) {
        Some(indice) => println!("El elemento {} se encuentra en el índice: {}", objetivo2, indice),
        None => println!("El elemento {} no se encontró en la lista.", objetivo2),
    }
}

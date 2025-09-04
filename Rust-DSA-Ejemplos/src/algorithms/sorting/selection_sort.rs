//! Módulo para demostrar el algoritmo de Ordenamiento por Selección (Selection Sort) en Rust.

// Ordena un slice de enteros utilizando el método de selección.
pub fn selection_sort(arr: &mut [i32]) {
    let n = arr.len();

    // Recorrer toda la lista
    for i in 0..n {
        // Encontrar el mínimo en la sublista no ordenada
        let mut min_idx = i;
        for j in i + 1..n {
            if arr[j] < arr[min_idx] {
                min_idx = j;
            }
        }
        // Intercambiar el elemento mínimo encontrado con el primer elemento de la sublista no ordenada
        arr.swap(i, min_idx);
    }
}

// Función de demostración para el Ordenamiento por Selección
pub fn selection_sort_demo() {
    let mut mi_lista_desordenada = [64, 25, 12, 22, 11];

    println!("Lista desordenada: {:?}", mi_lista_desordenada);

    selection_sort(&mut mi_lista_desordenada);

    println!("Lista ordenada:    {:?}", mi_lista_desordenada);

    let mut otra_lista = [38, 27, 43, 3, 9, 82, 10];
    println!("\nLista desordenada: {:?}", otra_lista);
    selection_sort(&mut otra_lista);
    println!("Lista ordenada:    {:?}", otra_lista);
}


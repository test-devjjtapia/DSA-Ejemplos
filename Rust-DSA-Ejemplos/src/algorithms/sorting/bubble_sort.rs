//! Módulo para demostrar el algoritmo de Ordenamiento de Burbuja (Bubble Sort) en Rust.

// Ordena un slice de enteros utilizando el método de la burbuja.
pub fn bubble_sort(arr: &mut [i32]) {
    let n = arr.len();
    // Recorremos todos los elementos del arreglo
    for i in 0..n {
        // Los últimos i elementos ya están en su lugar
        for j in 0..n - 1 - i {
            // Comparamos elementos adyacentes y los intercambiamos si están en el orden incorrecto
            if arr[j] > arr[j + 1] {
                arr.swap(j, j + 1);
            }
        }
    }
}

// Función de demostración para el Ordenamiento de Burbuja
pub fn bubble_sort_demo() {
    let mut mi_lista_desordenada = [64, 34, 25, 12, 22, 11, 90];

    println!("Lista desordenada: {:?}", mi_lista_desordenada);

    bubble_sort(&mut mi_lista_desordenada);

    println!("Lista ordenada:    {:?}", mi_lista_desordenada);

    let mut otra_lista = [5, 1, 4, 2, 8];
    println!("\nLista desordenada: {:?}", otra_lista);
    bubble_sort(&mut otra_lista);
    println!("Lista ordenada:    {:?}", otra_lista);
}

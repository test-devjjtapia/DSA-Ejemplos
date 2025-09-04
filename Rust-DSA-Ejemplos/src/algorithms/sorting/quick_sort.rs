//! Módulo para demostrar el algoritmo de Ordenamiento Rápido (Quick Sort) en Rust.

// Ordena un slice de enteros utilizando el método Quick Sort.
pub fn quick_sort(arr: &mut [i32]) {
    let n = arr.len();
    if n <= 1 {
        return;
    }
    quick_sort_recursive(arr, 0, (n - 1) as isize);
}

// Función auxiliar recursiva para Quick Sort
fn quick_sort_recursive(arr: &mut [i32], low: isize, high: isize) {
    if low < high {
        let pi = partition(arr, low, high);

        quick_sort_recursive(arr, low, pi - 1);
        quick_sort_recursive(arr, pi + 1, high);
    }
}

// Función auxiliar para particionar el slice
fn partition(arr: &mut [i32], low: isize, high: isize) -> isize {
    let pivot = arr[high as usize];
    let mut i = low - 1; // Índice del elemento más pequeño

    for j in low..high {
        // Si el elemento actual es menor o igual que el pivote
        if arr[j as usize] <= pivot {
            i += 1;
            arr.swap(i as usize, j as usize);
        }
    }
    arr.swap((i + 1) as usize, high as usize); // Intercambia el pivote
    i + 1
}

// Función de demostración para el Ordenamiento Rápido
pub fn quick_sort_demo() {
    let mut mi_lista_desordenada = [10, 7, 8, 9, 1, 5];

    println!("Lista desordenada: {:?}", mi_lista_desordenada);

    quick_sort(&mut mi_lista_desordenada);

    println!("Lista ordenada:    {:?}", mi_lista_desordenada);

    let mut otra_lista = [64, 34, 25, 12, 22, 11, 90];
    println!("\nLista desordenada: {:?}", otra_lista);
    quick_sort(&mut otra_lista);
    println!("Lista ordenada:    {:?}", otra_lista);
}

//! Módulo para demostrar el algoritmo de Ordenamiento por Inserción (Insertion Sort) en Rust.

// Ordena un slice de enteros utilizando el método de inserción.
pub fn insertion_sort(arr: &mut [i32]) {
    let n = arr.len();
    // Recorremos desde el segundo elemento
    for i in 1..n {
        let key = arr[i]; // El elemento que vamos a insertar
        let mut j = i as isize - 1;

        // Mover los elementos de arr[0..i-1] que son mayores que key
        // a una posición adelante de su posición actual
        while j >= 0 && arr[j as usize] > key {
            arr[(j + 1) as usize] = arr[j as usize];
            j -= 1;
        }
        arr[(j + 1) as usize] = key;
    }
}

// Función de demostración para el Ordenamiento por Inserción
pub fn insertion_sort_demo() {
    let mut mi_lista_desordenada = [12, 11, 13, 5, 6];

    println!("Lista desordenada: {:?}", mi_lista_desordenada);

    insertion_sort(&mut mi_lista_desordenada);

    println!("Lista ordenada:    {:?}", mi_lista_desordenada);

    let mut otra_lista = [64, 34, 25, 12, 22, 11, 90];
    println!("\nLista desordenada: {:?}", otra_lista);
    insertion_sort(&mut otra_lista);
    println!("Lista ordenada:    {:?}", otra_lista);
}

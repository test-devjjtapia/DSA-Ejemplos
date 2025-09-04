//! Módulo para demostrar el algoritmo de Ordenamiento por Fusión (Merge Sort) en Rust.

// Ordena un slice de enteros utilizando el método Merge Sort.
pub fn merge_sort(arr: &mut [i32]) {
    let n = arr.len();
    if n <= 1 {
        return;
    }

    let mid = n / 2;
    // Dividir los elementos en dos mitades
    let (left, right) = arr.split_at_mut(mid);

    // Ordenar la primera mitad recursivamente
    merge_sort(left);
    // Ordenar la segunda mitad recursivamente
    merge_sort(right);

    // Fusionar las dos mitades ordenadas
    merge(left, right);
}

// Función auxiliar para fusionar dos slices ordenados en uno solo.
fn merge(left: &mut [i32], right: &mut [i32]) {
    // Crear una copia temporal de los datos para la fusión
    let mut temp = Vec::with_capacity(left.len() + right.len());

    let mut i = 0; // Índice para el slice izquierdo
    let mut j = 0; // Índice para el slice derecho

    // Copiar datos a los arreglos temporales
    while i < left.len() && j < right.len() {
        if left[i] < right[j] {
            temp.push(left[i]);
            i += 1;
        } else {
            temp.push(right[j]);
            j += 1;
        }
    }

    // Comprobar si quedó algún elemento en el slice izquierdo
    while i < left.len() {
        temp.push(left[i]);
        i += 1;
    }

    // Comprobar si quedó algún elemento en el slice derecho
    while j < right.len() {
        temp.push(right[j]);
        j += 1;
    }

    // Copiar los elementos fusionados de vuelta al slice original
    for k in 0..temp.len() {
        if k < left.len() {
            left[k] = temp[k];
        } else {
            right[k - left.len()] = temp[k];
        }
    }
}

// Función de demostración para el Ordenamiento por Fusión
pub fn merge_sort_demo() {
    let mut mi_lista_desordenada = [38, 27, 43, 3, 9, 82, 10];

    println!("Lista desordenada: {:?}", mi_lista_desordenada);

    merge_sort(&mut mi_lista_desordenada);

    println!("Lista ordenada:    {:?}", mi_lista_desordenada);
}

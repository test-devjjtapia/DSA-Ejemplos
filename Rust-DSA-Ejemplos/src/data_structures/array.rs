//! Módulo para demostrar el uso de Vec (arreglos dinámicos) en Rust.

pub fn array_demo() {
    // --- 1. Creación y Acceso ---
    println!("--- Creación y Acceso ---");
    // En Rust, `Vec` es el equivalente a las listas dinámicas de Python
    let mut frutas = vec!["manzana", "banana", "cereza", "dátil"];
    println!("Lista completa: {:?}", frutas);

    // Acceso por índice (basado en 0)
    println!("Primer elemento (índice 0): {}", frutas[0]);
    println!("Tercer elemento (índice 2): {}", frutas[2]);
    println!("Último elemento (índice {}): {}", frutas.len() - 1, frutas[frutas.len() - 1]);

    // --- 2. Modificación ---
    println!("\n--- Modificación ---");
    frutas[1] = "arándano";
    println!("Lista después de modificar el índice 1: {:?}", frutas);

    // --- 3. Añadir elementos ---
    println!("\n--- Añadir elementos ---");
    // Añadir al final
    frutas.push("frambuesa");
    println!("Después de push('frambuesa'): {:?}", frutas);

    // Insertar en una posición específica
    frutas.insert(2, "kiwi"); // Inserta 'kiwi' en el índice 2
    println!("Después de insert(2, 'kiwi'): {:?}", frutas);

    // --- 4. Eliminar elementos ---
    println!("\n--- Eliminar elementos ---");
    // Eliminar el último elemento
    let elemento_eliminado = frutas.pop().unwrap(); // pop() retorna Option<T>
    println!("Elemento eliminado con pop(): {}", elemento_eliminado);
    println!("Lista actual: {:?}", frutas);

    // Eliminar por índice
    let elemento_eliminado_indice = frutas.remove(1); // Elimina el elemento en el índice 1
    println!("Elemento eliminado con remove(1): {}", elemento_eliminado_indice);
    println!("Lista actual: {:?}", frutas);

    // Eliminar por valor (la primera ocurrencia)
    // En Rust, esto a menudo se hace iterando y reconstruyendo el Vec o usando retain
    let mut index_to_remove: Option<usize> = None;
    for (i, &item) in frutas.iter().enumerate() {
        if item == "cereza" {
            index_to_remove = Some(i);
            break;
        }
    }
    if let Some(i) = index_to_remove {
        frutas.remove(i);
    }
    println!("Después de eliminar 'cereza': {:?}", frutas);

    // --- 5. Slicing (Rebanado) ---
    println!("\n--- Slicing ---");
    let numeros = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    println!("Lista de números: {:?}", numeros);

    // Obtener un sub-arreglo (del índice 2 al 4)
    let sub_lista = &numeros[2..5]; // [inicio..fin] - fin es exclusivo
    println!("Sub-lista de [2:5]: {:?}", sub_lista);

    // Desde el inicio hasta el índice 3
    let desde_inicio = &numeros[..4];
    println!("Sub-lista de [:4]: {:?}", desde_inicio);

    // Desde el índice 5 hasta el final
    let hasta_final = &numeros[5..];
    println!("Sub-lista de [5:]: {:?}", hasta_final);
}

//! Módulo para demostrar la inversión de una lista utilizando recursión en Rust.

// Invierte una lista utilizando un enfoque recursivo.
pub fn reverse_list_recursive<T: Clone>(lst: Vec<T>) -> Vec<T> {
    // Caso base: si la lista está vacía o tiene un solo elemento, ya está invertida.
    if lst.len() <= 1 {
        return lst;
    }

    // Paso recursivo:
    // 1. Toma el primer elemento (lst[0])
    // 2. Llama recursivamente a la función con el resto de la lista (lst[1..])
    // 3. Concatena el primer elemento al final de la lista invertida devuelta.
    let first_element = lst[0].clone();
    let mut reversed_tail = reverse_list_recursive(lst[1..].to_vec());
    reversed_tail.push(first_element);
    reversed_tail
}

// Función de demostración para la inversión de lista recursiva
pub fn list_reversal_demo() {
    let mi_lista = vec!["1", "2", "3", "4", "5"];
    println!("Lista original: {:?}", mi_lista);

    let lista_invertida = reverse_list_recursive(mi_lista);
    println!("Lista invertida: {:?}", lista_invertida);

    // Ejemplo con otra lista
    let otra_lista = vec!["a", "b", "c", "d"];
    println!("\nLista original: {:?}", otra_lista);
    let lista_invertida_2 = reverse_list_recursive(otra_lista);
    println!("Lista invertida: {:?}", lista_invertida_2);
}

#include <iostream>
#include <vector>
#include <string>
#include <algorithm> // Para std::reverse

// Invierte un vector utilizando un enfoque recursivo.
std::vector<std::string> reverseListRecursive(const std::vector<std::string>& lst) {
    // Caso base: si la lista está vacía o tiene un solo elemento, ya está invertida.
    if (lst.size() <= 1) {
        return lst;
    }

    // Paso recursivo:
    // 1. Toma el primer elemento (lst[0])
    // 2. Llama recursivamente a la función con el resto de la lista (lst[1..])
    // 3. Concatena el primer elemento al final de la lista invertida devuelta.
    std::string first_element = lst[0];
    std::vector<std::string> sub_list(lst.begin() + 1, lst.end());
    std::vector<std::string> reversed_tail = reverseListRecursive(sub_list);
    reversed_tail.push_back(first_element);
    return reversed_tail;
}

void list_reversal_demo() {
    std::vector<std::string> mi_lista = {"1", "2", "3", "4", "5"};
    std::cout << "Lista original: ";
    for (const std::string& s : mi_lista) {
        std::cout << s << " ";
    }
    std::cout << std::endl;

    std::vector<std::string> lista_invertida = reverseListRecursive(mi_lista);
    std::cout << "Lista invertida: ";
    for (const std::string& s : lista_invertida) {
        std::cout << s << " ";
    }
    std::cout << std::endl;

    // Ejemplo con otra lista
    std::vector<std::string> otra_lista = {"a", "b", "c", "d"};
    std::cout << "\nLista original: ";
    for (const std::string& s : otra_lista) {
        std::cout << s << " ";
    }
    std::cout << std::endl;
    std::vector<std::string> lista_invertida_2 = reverseListRecursive(otra_lista);
    std::cout << "Lista invertida: ";
    for (const std::string& s : lista_invertida_2) {
        std::cout << s << " ";
    }
    std::cout << std::endl;
}

int main() {
    list_reversal_demo();
    return 0;
}

//! Módulo para demostrar la implementación de un Árbol de Búsqueda Binaria (BST) en Rust.

use std::fmt;

// Definición de un Nodo del Árbol
pub struct TreeNode {
    pub key: i32,
    pub left: Option<Box<TreeNode>>,
    pub right: Option<Box<TreeNode>>,
}

impl TreeNode {
    pub fn new(key: i32) -> Self {
        TreeNode {
            key,
            left: None,
            right: None,
        }
    }
}

// Definición del Árbol de Búsqueda Binaria
pub struct BinarySearchTree {
    pub root: Option<Box<TreeNode>>,
}

impl BinarySearchTree {
    // Constructor para crear un nuevo BST vacío
    pub fn new() -> Self {
        BinarySearchTree { root: None }
    }

    // Inserta una nueva clave en el BST
    pub fn insert(&mut self, key: i32) {
        self.root = insert_recursive(self.root.take(), key);
    }

    // Función auxiliar recursiva para insertar
    fn insert_recursive(node: Option<Box<TreeNode>>, key: i32) -> Option<Box<TreeNode>> {
        match node {
            Some(mut n) => {
                if key < n.key {
                    n.left = insert_recursive(n.left.take(), key);
                } else if key > n.key {
                    n.right = insert_recursive(n.right.take(), key);
                }
                Some(n)
            }
            None => Some(Box::new(TreeNode::new(key))),
        }
    }

    // Busca una clave en el BST
    pub fn search(&self, key: i32) -> bool {
        search_recursive(self.root.as_ref(), key)
    }

    // Función auxiliar recursiva para buscar
    fn search_recursive(node: Option<&Box<TreeNode>>, key: i32) -> bool {
        match node {
            Some(n) => {
                if key == n.key {
                    true
                } else if key < n.key {
                    search_recursive(n.left.as_ref(), key)
                } else {
                    search_recursive(n.right.as_ref(), key)
                }
            }
            None => false,
        }
    }

    // Realiza un recorrido In-Order y retorna los elementos en un Vec
    pub fn inorder_traversal(&self) -> Vec<i32> {
        let mut result = Vec::new();
        inorder_traversal_recursive(self.root.as_ref(), &mut result);
        result
    }

    // Función auxiliar recursiva para el recorrido In-Order
    fn inorder_traversal_recursive(node: Option<&Box<TreeNode>>, result: &mut Vec<i32>) {
        if let Some(n) = node {
            inorder_traversal_recursive(n.left.as_ref(), result);
            result.push(n.key);
            inorder_traversal_recursive(n.right.as_ref(), result);
        }
    }
}

// Función de demostración para el BST
pub fn binary_search_tree_demo() {
    let mut bst = BinarySearchTree::new();
    println!("---" Insertando elementos en el BST ---");
    let keys = [50, 30, 20, 40, 70, 60, 80];
    for key in keys.iter() {
        bst.insert(*key);
        println!("Insertado: {}", key);
    }

    println!("\n---" Búsquedas en el BST ---");
    let search_key = 40;
    if bst.search(search_key) {
        println!("Clave {} encontrada.", search_key);
    } else {
        println!("Clave {} no encontrada.", search_key);
    }

    let search_key = 90;
    if bst.search(search_key) {
        println!("Clave {} encontrada.", search_key);
    } else {
        println!("Clave {} no encontrada.", search_key);
    }

    println!("\n---" Recorrido In-Order (debe salir ordenado) ---");
    println!("{:?}", bst.inorder_traversal());
}

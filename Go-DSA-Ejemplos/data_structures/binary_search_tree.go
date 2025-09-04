package data_structures

import "fmt"

type TreeNode struct {
	key   int
	left  *TreeNode
	right *TreeNode
}

type BinarySearchTree struct {
	root *TreeNode
}

func (bst *BinarySearchTree) Insert(key int) {
	bst.root = insertRecursive(bst.root, key)
}

func insertRecursive(current *TreeNode, key int) *TreeNode {
	if current == nil {
		return &TreeNode{key: key}
	}

	if key < current.key {
		current.left = insertRecursive(current.left, key)
	} else if key > current.key {
		current.right = insertRecursive(current.right, key)
	} else {
		// value already exists
		return current
	}
	return current
}

func (bst *BinarySearchTree) Search(key int) bool {
	return searchRecursive(bst.root, key)
}

func searchRecursive(current *TreeNode, key int) bool {
	if current == nil {
		return false
	}
	if key == current.key {
		return true
	}
	if key < current.key {
		return searchRecursive(current.left, key)
	}
	return searchRecursive(current.right, key)
}

func (bst *BinarySearchTree) InOrderTraversal() []int {
	result := []int{}
	inOrderTraversalRecursive(bst.root, &result)
	return result
}

func inOrderTraversalRecursive(node *TreeNode, result *[]int) {
	if node != nil {
		inOrderTraversalRecursive(node.left, result)
		*result = append(*result, node.key)
		inOrderTraversalRecursive(node.right, result)
	}
}

func BinarySearchTreeDemo() {
	bst := BinarySearchTree{}
	fmt.Println("--- Insertando elementos en el BST ---")
	keys := []int{50, 30, 20, 40, 70, 60, 80}
	for _, key := range keys {
		bst.Insert(key)
		fmt.Println("Insertado:", key)
	}

	fmt.Println("\n--- Búsquedas en el BST ---")
	searchKey := 40
	if bst.Search(searchKey) {
		fmt.Println("Clave", searchKey, "encontrada.")
	} else {
		fmt.Println("Clave", searchKey, "no encontrada.")
	}

	searchKey = 90
	if bst.Search(searchKey) {
		fmt.Println("Clave", searchKey, "encontrada.")
	} else {
		fmt.Println("Clave", searchKey, "no encontrada.")
	}

	fmt.Println("\n--- Recorrido In-Order (debe salir ordenado) ---")
	fmt.Println(bst.InOrderTraversal())
}

func main() {
	BinarySearchTreeDemo()
}

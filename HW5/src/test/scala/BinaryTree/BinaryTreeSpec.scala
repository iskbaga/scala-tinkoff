package BinaryTree

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec
import scala.util.Random

class BinaryTreeSpec extends AnyWordSpec with Matchers {
  def isBinarySearchTree(node: Node): Boolean = (node.value, node.left, node.right) match {
    case (_, None, None)            => true
    case (value, Some(left), None)  => value > left.value && isBinarySearchTree(left)
    case (value, None, Some(right)) => value <= right.value && isBinarySearchTree(right)
    case (value, Some(left), Some(right)) =>
      value > left.value && value <= right.value && isBinarySearchTree(left) && isBinarySearchTree(right)
    case _ => false
  }

  def elementCheck(node: Option[Node], value: Int): Boolean = node match {
    case None => false
    case Some(node) =>
      if (node.value == value) true
      else if (value < node.value) elementCheck(node.left, value)
      else elementCheck(node.right, value)
  }

  def height(node: Option[Node]): Int = node match {
    case None    => 0
    case Some(x) => x.height
  }

  def isBalanced(node: Option[Node]): Boolean = node match {
    case None => true
    case Some(n) =>
      val leftHeight = height(n.left)
      val rightHeight = height(n.right)
      val balanceFactor = Math.abs(leftHeight - rightHeight)
      balanceFactor <= 1 && isBalanced(n.left) && isBalanced(n.right)
  }

  val root1: Node = new Node(
    4,
    Some(new Node(2, Some(new Node(1)), Some(new Node(3)), 2)),
    Some(new Node(7, Some(new Node(5, None, Some(new Node(6)), 2)), Some(new Node(8, None, Some(new Node(11)), 2)), 3)),
    4
  )
  val root2: Node = new Node(
    13,
    Some(new Node(2, Some(new Node(-1, None, Some(new Node(-6)), 2)), Some(new Node(3, height = 2)), 3)),
    Some(
      new Node(
        4,
        Some(new Node(14, Some(new Node(12)), Some(new Node(18)), 2)),
        Some(new Node(19, Some(new Node(15)), Some(new Node(20)), 2)),
        3
      )
    ),
    4
  )

  val tree1 = new BinaryTree(root1)
  val tree2 = new BinaryTree(root2)

  "Binary tree" should {

    "Add method should correctly insert a value into the tree" in {
      @tailrec
      def tempCheck(tree: BinaryTree, iter: Int): Unit = {
        if (iter < 100) {
          val randomValue = Random.nextInt(201)
          val newTree = tree.add(randomValue)
          // Можем вывести дерево после каждой операции для отладки(демонстрация сбалансированности)
          // println(newTree)
          elementCheck(Some(newTree.root), randomValue) shouldEqual true
          isBinarySearchTree(newTree.root) shouldEqual true
          isBalanced(Some(newTree.root)) shouldEqual true
          tempCheck(newTree, iter + 1)
        }
      }
      tempCheck(tree1, 0)
    }

    "Delete method should correctly remove a value from the AVL tree" in {
      def deleteCheck(tree: BinaryTree, value: Int): Unit = {
        val newTree = tree.delete(value)
        // Можем вывести дерево после каждой операции для отладки(демонстрация сбалансированности)
        // println(newTree)
        elementCheck(Some(newTree.root), value) shouldEqual false
        isBinarySearchTree(newTree.root) shouldEqual true
        isBalanced(Some(newTree.root)) shouldEqual true
      }
      deleteCheck(tree1, 4) // удаление значения корня
      deleteCheck(tree1, 5) // удаление вершины с одним ребенком
      deleteCheck(tree1, 7) // удаление вершины с двумя детьми
      deleteCheck(tree1, 1) // удаление листа
    }

    "foldLeft method should correctly accumulate values in the tree" in {
      tree1.foldLeft(0)((acc, node) => acc + node.value) shouldEqual 47
      tree1.foldLeft(40)((acc, node) => acc - node.value) shouldEqual -7
      tree1.foldLeft(1)((acc, node) => acc * node.value) shouldEqual 443520

      tree2.foldLeft(0)((acc, node) => acc + node.value) shouldEqual 113
      tree2.foldLeft(1)((acc, node) => acc - node.value) shouldEqual -112
      tree2.foldLeft(1)((acc, node) => acc * node.value) shouldEqual -2092448768
    }

    "breadthFirstSearch method should return nodes in BFS order" in {
      tree1.breadthFirstSearch().map(node => node.value) shouldEqual List(4, 2, 7, 1, 3, 5, 8, 6, 11)
      tree2.breadthFirstSearch().map(node => node.value) shouldEqual List(13, 2, 4, -1, 3, 14, 19, -6, 12, 18, 15, 20)
    }

    "depthFirstSearch method should return nodes in DFS order" in {
      tree1.depthFirstSearch().map(node => node.value) shouldEqual List(4, 2, 1, 3, 7, 5, 8, 6, 11)
      tree2.depthFirstSearch().map(node => node.value) shouldEqual List(13, 2, -1, 3, 4, 14, 19, -6, 12, 18, 15, 20)
    }

    "max method should correctly find the maximum value in the tree using BFS and DFS" in {
      tree1.max(tree1.depthFirstSearch) shouldEqual 11
      tree1.max(tree1.breadthFirstSearch) shouldEqual 11
      tree2.max(tree2.depthFirstSearch) shouldEqual 20
      tree2.max(tree2.breadthFirstSearch) shouldEqual 20
    }

    "min method should correctly find the minimum value in the tree using BFS and DFS" in {
      tree1.min(tree1.depthFirstSearch) shouldEqual 1
      tree1.min(tree1.breadthFirstSearch) shouldEqual 1
      tree2.min(tree2.depthFirstSearch) shouldEqual -6
      tree2.min(tree2.breadthFirstSearch) shouldEqual -6
    }

    "size method should correctly return the number of nodes in the tree" in {
      @tailrec
      def sizeCheck(tree: BinaryTree, sz: Int, iter: Int): Unit = {
        if (iter > 0) {
          val newTree = tree.add(iter + 100)
          newTree.size() shouldEqual sz + 1
          sizeCheck(newTree, sz + 1, iter - 1)
        }
      }

      sizeCheck(tree1, 9, 100)
      sizeCheck(tree2, 12, 100)
    }
  }

}

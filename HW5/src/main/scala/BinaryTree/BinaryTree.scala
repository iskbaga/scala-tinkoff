package BinaryTree

import scala.annotation.tailrec

class Node(val value: Int, val left: Option[Node] = None, val right: Option[Node] = None, val height: Int = 1)

class BinaryTree(val root: Node) {

  def add(value: Int): BinaryTree = { // из-за того что дерево гарантированно сбалансированное переполнения стека не будет
    def addHelper(x: Option[Node], value: Int): Node = x match {
      case None => new Node(value, None, None)
      case Some(node) =>
        if (value == node.value) {
          node
        } else {
          val tempNode: Node = {
            if (value < node.value) {
              val addedLeft: Option[Node] = Some(addHelper(node.left, value))
              val newHeight: Int = updateHeight(addedLeft, node.right)
              new Node(node.value, addedLeft, node.right, newHeight)
            } else {
              val addedRight: Option[Node] = Some(addHelper(node.right, value))
              val newHeight: Int = updateHeight(node.left, addedRight)
              new Node(node.value, node.left, addedRight, newHeight)
            }
          }

          val balance = balanceFactor(Some(tempNode))
          val (leftVal, rightVal): (Int, Int) = (tempNode.left, tempNode.right) match {
            case (None, None)               => (1, 1)
            case (None, Some(node))         => (1, node.value)
            case (Some(node), None)         => (node.value, 1)
            case (Some(node1), Some(node2)) => (node1.value, node2.value)
          }
          if (balance > 1 && value < leftVal) {
            rightRotate(tempNode)
          } else if (balance < -1 && value > rightVal) {
            leftRotate(tempNode)
          } else if (balance > 1 && value > leftVal) {
            tempNode.left match {
              case None => rightRotate(new Node(tempNode.value, None, tempNode.right, tempNode.height))
              case Some(leftNode) =>
                rightRotate(new Node(tempNode.value, Some(leftRotate(leftNode)), tempNode.right, tempNode.height))
            }
          } else if (balance < -1 && value < rightVal) {
            tempNode.right match {
              case None => leftRotate(new Node(tempNode.value, tempNode.left, None, tempNode.height))
              case Some(rightNode) =>
                leftRotate(new Node(tempNode.value, tempNode.left, Some(rightRotate(rightNode)), tempNode.height))
            }
          } else {
            tempNode
          }
        }
    }
    new BinaryTree(addHelper(Some(root), value))
  }

  def delete(value: Int): BinaryTree = {
    def deleteHelper(x: Option[Node], value: Int): Option[Node] = x match {
      case None => None
      case Some(node) => {
        (node.left, node.right) match {
          case (None, None)       => None
          case (None, Some(temp)) => Some(temp)
          case (Some(temp), None) => Some(temp)
          case (Some(left), Some(right)) =>
            val newNode: Node = {
              if (value < node.value) {
                val deletedLeft: Option[Node] = deleteHelper(Some(left), value)
                val newHeight: Int = updateHeight(deletedLeft, Some(right))
                new Node(node.value, deletedLeft, Some(right), newHeight)
              } else if (value > node.value) {
                val deletedRight: Option[Node] = deleteHelper(Some(right), value)
                val newHeight: Int = updateHeight(Some(left), deletedRight)
                new Node(node.value, Some(left), deletedRight, newHeight)
              } else {
                val temp = minValueNode(right)
                val deletedRight: Option[Node] = deleteHelper(Some(right), temp.value)
                val newHeight: Int = updateHeight(Some(left), deletedRight)
                new Node(minValueNode(right).value, Some(left), deletedRight, newHeight)
              }
            }

            val balance = balanceFactor(Some(newNode))

            if (balance > 1 && balanceFactor(newNode.left) >= 0) {
              Some(rightRotate(newNode))
            } else if (balance > 1 && balanceFactor(newNode.left) < 0) {
              newNode.left match {
                case None => Some(rightRotate(new Node(newNode.value, None, newNode.right, newNode.height)))
                case Some(leftNode) =>
                  Some(
                    rightRotate(new Node(newNode.value, Some(leftRotate(leftNode)), newNode.right, newNode.height))
                  )
              }
            } else if (balance < -1 && balanceFactor(newNode.right) <= 0) {
              Some(leftRotate(newNode))
            } else if (balance < -1 && balanceFactor(newNode.right) > 0) {
              newNode.right match {
                case None => Some(leftRotate(new Node(newNode.value, newNode.left, None, newNode.height)))
                case Some(rightVal) =>
                  Some(
                    leftRotate(new Node(newNode.value, newNode.left, Some(rightRotate(rightVal)), newNode.height))
                  )
              }
            } else {
              Some(newNode)
            }
        }
      }
    }
    deleteHelper(Some(root), value) match {
      case None       => new BinaryTree(new Node(0, None, None)) // так как дерево по условию не может быть пустым
      case Some(tree) => new BinaryTree(tree)
    }
  }

  @tailrec
  private def minValueNode(node: Node): Node = node.left match {
    case None    => node
    case Some(l) => minValueNode(l)
  }

  def foldLeft[A](startAccum: A, startNode: Option[Node] = Some(root))(f: (A, Node) => A): A = {
    @tailrec
    def foldHelper(queue: List[Option[Node]], accum: A): A = {
      if (queue.isEmpty) {
        accum
      } else {
        queue.head match {
          case Some(node) =>
            val newValue = f(accum, node)
            val newQueue = queue.tail :+ node.left :+ node.right
            foldHelper(newQueue, newValue)
          case None =>
            foldHelper(queue.tail, accum)
        }
      }
    }

    foldHelper(List(startNode), startAccum)
  }

  def depthFirstSearch(): List[Node] = {
    def dfsHelper(node: Node, visited: List[Node]): List[Node] = {
      if (visited.contains(node)) visited
      else {
        foldLeft(visited :+ node, Some(node)) { (accum, currentNode) =>
          if (!accum.contains(currentNode)) {
            val left = currentNode.left.filterNot(accum.contains)
            val right = currentNode.right.filterNot(accum.contains)
            accum ++ List(currentNode) ++ List(left, right).flatten
          } else {
            accum
          }
        }
      }
    }

    foldLeft[List[Node]](List.empty[Node])((list, node) => dfsHelper(node, list))
  }

  def breadthFirstSearch(): List[Node] = {
    foldLeft[List[Node]](List.empty[Node])((queue, node) => queue :+ node)
  }

  def max(f: () => List[Node]): Int = {
    val values = f().map(x => x.value)

    @tailrec
    def maxHelper(list: List[Int], result: Int): Int = {
      list.headOption match {
        case None => result
        case Some(value) =>
          if (value > result) {
            maxHelper(list.tail, value)
          } else {
            maxHelper(list.tail, result)
          }
      }
    }

    maxHelper(values, Int.MinValue)
  }

  // такой код только из-за условия, очевидно что минимум это первый элемент dfs
  def min(f: () => List[Node]): Int = {
    val values = f().map(x => x.value)

    @tailrec
    def minHelper(list: List[Int], result: Int): Int = {
      list.headOption match {
        case None => result
        case Some(value) =>
          if (value >= result) {
            minHelper(list.tail, result)
          } else {
            minHelper(list.tail, value)
          }
      }
    }

    minHelper(values, Int.MaxValue)
  }

  def size(): Int = {
    foldLeft[Int](0)((accum: Int, _: Node) => accum + 1)
  }

  override def toString: String = { // реализация без foldLeft так как я хочу проходиться по несуществующим вершинам
    def collectLevel(currentLevel: List[Option[Node]]): (String, List[Option[Node]]) = {
      @tailrec
      def levelHelper(
        result: String,
        next: List[Option[Node]],
        current: List[Option[Node]]
      ): (String, List[Option[Node]]) = {
        if (current.isEmpty) {
          (result, next)
        } else {
          current.head match {
            case None    => levelHelper(result + "_ ", next :+ None :+ None, current.tail)
            case Some(x) => levelHelper(result + x.value + " ", next :+ x.left :+ x.right, current.tail)
          }
        }
      }

      levelHelper("", List.empty[Option[Node]], currentLevel)
    }

    @tailrec
    def loop(currentLevel: List[Option[Node]], levels: List[String]): String = {
      if (currentLevel.flatten.nonEmpty) {
        val (values, next) = collectLevel(currentLevel)
        loop(next, levels :+ values)
      } else {
        levels.mkString("\n")
      }
    }

    loop(List(Some(root)), List.empty)
  }

  def inOrderPrint(): Unit = { // вывод всех вершин в строку
    def printHelper(node: Option[Node]): Unit = {
      node.foreach { x =>
        printHelper(x.left)
        print(s"${x.value} ")
        printHelper(x.right)
      }
    }

    printHelper(Some(root))
  }

  def printTree(): Unit = { // альтернативный принт слева направо
    def printHelper(p: Option[Node], level: Int): Unit = {
      p.foreach { x =>
        printHelper(x.right, level + 1);
        print("   " * level);
        println(x.height)
        printHelper(x.left, level + 1);
      }
    }

    printHelper(Some(root), 0)
  }

  // вспомогательные функции для балансировки дерева
  private def height(node: Option[Node]): Int = node match {
    case None    => 0
    case Some(x) => x.height
  }

  private def updateHeight(left: Option[Node], right: Option[Node]): Int = {
    1 + Math.max(height(left), height(right))
  }

  private def balanceFactor(node: Option[Node]): Int = node match {
    case None    => 0
    case Some(x) => height(x.left) - height(x.right)
  }

  private def leftRotate(tree: Node): Node = tree.right match {
    case Some(right) =>
      new Node(
        right.value,
        Some(new Node(tree.value, tree.left, right.left, 1 + Math.max(height(tree.left), height(right.left)))),
        right.right,
        1 + Math.max(height(Some(new Node(tree.value, tree.left, right.left))), height(right.right))
      )
    case None => tree
  }

  private def rightRotate(tree: Node): Node = tree.left match {
    case Some(left) =>
      new Node(
        left.value,
        left.left,
        Some(new Node(tree.value, left.right, tree.right, 1 + Math.max(height(left.right), height(tree.right)))),
        1 + Math.max(height(left.left), height(left.right))
      )
    case None => tree
  }
}

package task1

sealed trait Tree[+T]

case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[+T](value: T) extends Tree[T]

package collections

import scala.annotation.tailrec

object Collections {
  /*
    In a sorted list find all pairs of two neighbor numbers which have a gap between them
    None for Seq(1, 2, 3, 4)
    Some(Seq((2, 8))) for Seq(1, 2, 8)
    Some(Seq((3, 5), (5, 7))) for Seq(3, 5, 7)
   */

  def findGaps(l: Seq[Int]): Option[Seq[(Int, Int)]] = {
    if (l.isEmpty) None
    else Some(l.zip(l.tail).filter(pair => pair._1 + 1 != pair._2))
  }

  /*
    Find key-value pair with the minimum value in the map
    try to implement min in different ways (fold, reduce, recursion)
   */
  def minFold(map: Map[String, Int]): Option[(String, Int)] = {
    if (map.isEmpty) None
    else Some(map.foldLeft(map.head)((first, second) => if (first._2 > second._2) second else first))
  }

  def minReduce(map: Map[String, Int]): Option[(String, Int)] = {
    if (map.isEmpty) None
    else Some(map.reduce((x, y) => if (y._2 > x._2) x else y))
  }

  def minRecursion(map: Map[String, Int]): Option[(String, Int)] = {
    @tailrec
    def minHelper(values: Map[String, Int], result: Option[(String, Int)]): Option[(String, Int)] = {
      if (values.isEmpty) result
      else {
        val current: (String, Int) = values.head
        result match {
          case None => minHelper(values.tail, Some(current))
          case Some(value) =>
            if (current._2 < value._2) minHelper(values.tail, Some(current))
            else minHelper(values.tail, Some(value))
        }
      }
    }

    if (map.isEmpty) None
    else minHelper(map.tail, Some(map.head))
  }

  // Implement scanLeft - running total, applying [f] to elements of [list] (not using scans ofc)
  def scanLeft[T](zero: T)(list: Seq[T])(f: (T, T) => T): Seq[T] = {
    @tailrec
    def scanHelper(accum: T, result: Seq[T], currentList: Seq[T]): Seq[T] = {
      if (currentList.isEmpty) result
      else {
        val value = f(accum, currentList.head)
        scanHelper(value, result :+ value, currentList.tail)
      }
    }
    scanHelper(zero, Seq(zero), list)
  }

  // Count the consistent occurences of each character in the string
  def count(s: String): List[(Char, Int)] = {
    val values: List[Char] = s.toList
    @tailrec
    def countHelper(currentList: List[Char], result: Map[Char, Int]): Map[Char, Int] = {
      if (currentList.isEmpty) result
      else {
        val current = currentList.head
        if (result.contains(current)) {
          countHelper(currentList.tail, result + (current -> (result(current) + 1)))
        } else {
          countHelper(currentList.tail, result + (current -> 1))
        }
      }
    }
    countHelper(values, Map.empty[Char, Int]).toList
  }
}

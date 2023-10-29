package ventilation

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class Ventilation(list: List[Int]) {
  def solve1(k: Int): List[Int] = {
    list.sliding(k).map(_.max).toList
  }

  def solve2(k: Int): List[Int] = {
    @tailrec
    def solveHelper(deque: Queue[Int], i: Int, acc: List[Int]): List[Int] = {
      if (i >= list.length) acc.reverse
      else { // O(n) итераций
        val filteredDeque = deque.dropWhile(index => index <= i - k) // амортизировано за O(1)
        val tempDeque =
          filteredDeque.reverse.dropWhile(index => list(index) <= list(i)).reverse // амортизировано за O(1)
        val updatedDeque = tempDeque.enqueue(i) // амортизировано за O(1)
        val updatedAcc = if (i >= k - 1) list(updatedDeque.head) :: acc else acc // O(1)

        solveHelper(updatedDeque, i + 1, updatedAcc)
      }
    }

    if (list.isEmpty) {
      List.empty[Int]
    } else if (k >= list.length) {
      List(
        list.max
      ) // O(n), для окна большего чем сам массив возвращаю макс значение чтобы унифицировать логику с solve1
    } else {
      solveHelper(Queue(), 0, List())
    }
  }

}

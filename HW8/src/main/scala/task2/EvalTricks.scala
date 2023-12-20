package task2
import cats.Eval

object EvalTricks {

  def fib(n: Int): Int = {
    def fibTailrec(a: Int, b: Int, n: Int): Eval[Int] =
      if (n > 0) Eval.defer(fibTailrec(b, a + b, n - 1))
      else Eval.now(a)

    fibTailrec(0, 1, n).value
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def foldRightRec(as: List[A], acc: B)(f: (A, B) => B): Eval[B] =
      Eval.now(as).flatMap {
        case head :: tail => Eval.defer(foldRightRec(tail, acc)(f).map(b => f(head, b)))
        case Nil          => Eval.later(acc)
      }
    foldRightRec(as, acc)(fn).value
  }
}

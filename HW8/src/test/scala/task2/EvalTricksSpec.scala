package task2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import task2.EvalTricks.{fib, foldRight}

class EvalTricksSpec extends AnyFlatSpec with Matchers {

  "fib" should "calculate fibonacci sequence correctly" in {
    def basicFib(n: Int): Int =
      if (n == 1 || n == 2) 1
      else basicFib(n - 1) + basicFib(n - 2)
    for (i <- 1 until 10) {
      fib(i) shouldEqual basicFib(i)
    }
  }

  "fib" should "work with long values" in {
    noException should be thrownBy fib(100001)
  }

  "foldRight" should "fold list correctly" in {
    foldRight(List(1, 2, 3, 4), -5)(_ + _) shouldEqual 5
  }

  "foldRight" should "work with long lists" in {
    val list = List.range(1, 100000)
    foldRight(list, 0)(_ + _) shouldEqual list.sum
  }

}

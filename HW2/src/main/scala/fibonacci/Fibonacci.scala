package fibonacci

import scala.annotation.tailrec

object Fibonacci {

  def fibonacci(limit: Long): BigInt = {
    @tailrec
    def fibHelper(n: Long, a: BigInt, b: BigInt): BigInt = {
      if (n == 0) a
      else if (n == 1) b
      else fibHelper(n - 1, b, a + b)
    }
    fibHelper(limit, 0, 1)
  }
}

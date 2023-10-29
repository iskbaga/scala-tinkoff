package ventilation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VentilationSpec extends AnyFlatSpec with Matchers {

  "solve1" should "return the maximum value for each sliding window of size k" in {
    val vent = new Ventilation(List(1, 2, 3, 4))

    vent.solve1(1) shouldEqual List(1, 2, 3, 4)
    vent.solve1(2) shouldEqual List(2, 3, 4)
    vent.solve1(3) shouldEqual List(3, 4)
  }

  "solve2" should "return the maximum value for each sliding window of size k" in {
    val vent = new Ventilation(List(1, 2, 3, 4))

    vent.solve2(1) shouldEqual List(1, 2, 3, 4)
    vent.solve2(2) shouldEqual List(2, 3, 4)
    vent.solve2(3) shouldEqual List(3, 4)
  }

  "solve1" should "return empty list if N == 0" in {
    val vent = new Ventilation(List())

    vent.solve1(1) shouldEqual List.empty
  }

  "solve2" should "return empty list if N == 0" in {
    val vent = new Ventilation(List())

    vent.solve2(1) shouldEqual List.empty
  }

  "solve1" should "work correctly with negative numbers" in {
    val vent = new Ventilation(List(2, -3, 1, 5, -1, 0, 3))

    vent.solve1(1) shouldEqual List(2, -3, 1, 5, -1, 0, 3)
    vent.solve1(2) shouldEqual List(2, 1, 5, 5, 0, 3)
    vent.solve1(3) shouldEqual List(2, 5, 5, 5, 3)
    vent.solve1(4) shouldEqual List(5, 5, 5, 5)
    vent.solve1(5) shouldEqual List(5, 5, 5)
    vent.solve1(6) shouldEqual List(5, 5)
    vent.solve1(7) shouldEqual List(5)
  }
  "solve2" should "work correctly with negative numbers" in {
    val vent = new Ventilation(List(2, -3, 1, 5, -1, 0, 3))

    vent.solve2(1) shouldEqual List(2, -3, 1, 5, -1, 0, 3)
    vent.solve2(2) shouldEqual List(2, 1, 5, 5, 0, 3)
    vent.solve2(3) shouldEqual List(2, 5, 5, 5, 3)
    vent.solve2(4) shouldEqual List(5, 5, 5, 5)
    vent.solve2(5) shouldEqual List(5, 5, 5)
    vent.solve2(6) shouldEqual List(5, 5)
    vent.solve2(7) shouldEqual List(5)
  }

  "solve1" should "return the maximum value in list when k is greater than the list size" in {
    val vent = new Ventilation(List(1, 2, -1, 4))

    vent.solve1(5) shouldEqual List(4)
  }

  "solve2" should "return the maximum value in list when k is greater than the list size" in {
    val vent = new Ventilation(List(1, 2, -1, 4))

    vent.solve2(5) shouldEqual List(4)
  }

}

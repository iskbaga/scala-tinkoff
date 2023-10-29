package collections

import collections.Collections._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionsSpec extends AnyFlatSpec with Matchers {

  "findGaps" should "return None for an empty list" in {
    findGaps(Seq()) shouldEqual None
  }

  it should "return empty list for a sorted list" in {
    findGaps(Seq(1, 2, 3, 4)) shouldEqual Some(List.empty)
  }

  it should "return Some(Seq((2, 8))) for Seq(1, 2, 8)" in {
    findGaps(Seq(1, 2, 8)) shouldEqual Some(Seq((2, 8)))
  }

  it should "return Some(Seq((3, 5), (5, 7))) for Seq(3, 5, 7)" in {
    findGaps(Seq(3, 5, 7)) shouldEqual Some(Seq((3, 5), (5, 7)))
  }

  "minFold" should "return None for an empty map" in {
    minFold(Map()) shouldEqual None
  }

  it should "return Some((key, value)) for a map with a single entry" in {
    minFold(Map("key" -> 42)) shouldEqual Some("key" -> 42)
  }

  it should "return the key-value pair with the minimum value in the map" in {
    minFold(Map("a" -> 5, "b" -> 3, "c" -> 7)) shouldEqual Some("b" -> 3)
  }

  "minReduce" should "return None for an empty map" in {
    minReduce(Map()) shouldEqual None
  }

  it should "return Some((key, value)) for a map with a single entry" in {
    minReduce(Map("key" -> 42)) shouldEqual Some("key" -> 42)
  }

  it should "return the key-value pair with the minimum value in the map" in {
    minReduce(Map("a" -> 5, "b" -> 3, "c" -> 7)) shouldEqual Some("b" -> 3)
  }

  "minRecursion" should "return None for an empty map" in {
    minRecursion(Map()) shouldEqual None
  }

  it should "return Some((key, value)) for a map with a single entry" in {
    minRecursion(Map("key" -> 42)) shouldEqual Some("key" -> 42)
  }

  it should "return the key-value pair with the minimum value in the map" in {
    minRecursion(Map("a" -> 5, "b" -> 3, "c" -> 7)) shouldEqual Some("b" -> 3)
  }

  "scanLeft" should "return a list with a running total" in {
    scanLeft(0)(Seq(1, 2, 3, 4))(_ + _) shouldEqual Seq(0, 1, 3, 6, 10)
    scanLeft(1)(Seq(1, 2, 3, 4))(_ * _) shouldEqual Seq(1, 1, 2, 6, 24)
  }

  "count" should "return a list of character counts in a string" in {
    count("hello") shouldEqual List(('h', 1), ('e', 1), ('l', 2), ('o', 1))
  }
}

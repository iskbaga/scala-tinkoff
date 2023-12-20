package task1.hierarchy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import task1.hierarchy.TypeClasses._
import task1.{Branch, Leaf, Tree}

class TypeClassesSpec extends AnyFlatSpec with Matchers {

  val tree1: Tree[Int] = Branch(Leaf(1), Leaf(2))
  val value1 = 42

  "treeMonad" should "satisfy left identity law" in {
    def f: Int => Tree[String] = (x: Int) => Leaf((x * 2).toString)
    def g: Int => Tree[Boolean] = (x: Int) => Leaf(x % 2 == 0)
    val left1 = treeMonad.flatMap(treeMonad.pure(value1))(f)
    val res1 = f(value1)
    val left2 = treeMonad.flatMap(treeMonad.pure(value1))(g)
    val res2 = g(value1)
    left1 shouldEqual res1
    left2 shouldEqual res2
  }

  "treeMonad" should "satisfy right identity law" in {
    val right1 = treeMonad.flatMap(tree1)(treeMonad.pure)
    val res1 = tree1
    right1 shouldEqual res1
  }

  "treeMonad" should "satisfy associativity law" in {
    val a = treeMonad.flatMap(tree1)(x => treeMonad.pure(x * 2))
    val b = treeMonad.flatMap(a)(x => treeMonad.pure(x.toString))
    val c = treeMonad.flatMap(tree1)(x => treeMonad.pure((x * 2).toString))
    b shouldEqual c
  }

  "pure" should "work correctly" in {
    val pureRes = treeMonad.pure(value1)
    val res = Leaf(value1)
    pureRes shouldEqual res
  }

  "map" should "work correctly" in {
    val mapRes = treeMonad.map(tree1)(_ * 2)
    val res = Branch(Leaf(2), Leaf(4))
    mapRes shouldEqual res
  }

  "ap" should "work correctly" in {
    val ff: Tree[Int => String] = Leaf(_.toString)
    val fa: Tree[Int] = Leaf(value1)
    val apRes = treeMonad.ap(ff)(fa)
    val res = Leaf(value1.toString)
    apRes shouldEqual res
  }

  "flatMap" should "work correctly" in {
    val flatMapRes = treeMonad.flatMap(tree1)(x => Leaf(x * 2))
    val res = Branch(Leaf(2), Leaf(4))
    flatMapRes shouldEqual res
  }
}

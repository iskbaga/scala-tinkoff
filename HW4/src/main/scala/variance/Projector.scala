package variance

import scala.util.Random

trait Converter[S] {
  def convert(value: S): String
}

trait Slide[+R] {
  def read: (Option[R], Slide[R])
}

class Projector[-R](converter: Converter[R]) {
  def project(screen: Slide[R]): String = {
    val (current, next) = screen.read
    current match {
      case None => ""
      case Some(current) =>
        converter.convert(current) + project(next)

    }
  }
}

class WordLine(val word: String)

class RedactedWordLine(val redactionFactor: Double, word: String) extends WordLine(word)

object LineConverter extends Converter[WordLine] {
  override def convert(value: WordLine): String = value.word + "\n"
}

object RedactedLineConverter extends Converter[RedactedWordLine] {
  override def convert(value: RedactedWordLine): String = {
    if (Random.nextDouble() < value.redactionFactor) {
      "█" * value.word.length + "\n"
    } else {
      value.word + "\n"
    }
  }
}

class HelloSlide[R <: WordLine](lines: Seq[R]) extends Slide[R] {
  private def readHelper(lines: Seq[R]): (Option[R], Seq[R]) = {
    lines match {
      case Nil          => (None, Nil)
      case head :: tail => (Some(head), tail)
      case _            => (None, Nil)
    }
  }
  val (current, next) = readHelper(lines)
  override def read: (Option[R], Slide[R]) = current match {
    case None =>
      val nextSlide = new HelloSlide(next)
      (None, nextSlide)
    case Some(current) =>
      val nextSlide = new HelloSlide(next)
      (Some(current), nextSlide)
  }
}

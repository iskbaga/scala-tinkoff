package variance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class ProjectorSpec extends AnyFlatSpec with Matchers {
  val wordLines1 = Seq(
    new WordLine("Hello"),
    new WordLine("World"),
    new WordLine("Data")
  )
  val wordLines2 = Seq(
    new RedactedWordLine(0.5, "Redacted"),
    new RedactedWordLine(0.8, "Hello"),
    new RedactedWordLine(0.3, "World"),
    new RedactedWordLine(1, "Data"),
    new RedactedWordLine(0, "Aaa")
  )
  val wordLines22 = Seq(
    new RedactedWordLine(0.5, "Redacted"),
    new RedactedWordLine(0.8, "Hello"),
    new RedactedWordLine(0.3, "World")
  )
  val wordLines31 = Seq(
    new RedactedWordLine(0.5, "Redacted"),
    new WordLine("Hello"),
    new WordLine("World"),
    new WordLine("Data")
  )
  val wordLines32 = Seq(
    new WordLine("Hello"),
    new RedactedWordLine(0.5, "Redacted"),
    new WordLine("World"),
    new WordLine("Data")
  )
  val wordLines33 = Seq(
    new WordLine("Hello"),
    new WordLine("World"),
    new RedactedWordLine(0.5, "Sensitive"),
    new WordLine("Data")
  )
  val wordLines34 = Seq(
    new WordLine("Hello"),
    new WordLine("World"),
    new WordLine("Data"),
    new RedactedWordLine(0.5, "Redacted")
  )
  val wordLines41 = Seq(
    new WordLine("Hello"),
    new RedactedWordLine(0.5, "World"),
    new RedactedWordLine(0.5, "Data"),
    new RedactedWordLine(0.5, "Redacted")
  )
  val wordLines42 = Seq(
    new RedactedWordLine(0.5, "World"),
    new WordLine("Hello"),
    new RedactedWordLine(0.5, "Data"),
    new RedactedWordLine(0.5, "Redacted")
  )
  val wordLines43 = Seq(
    new RedactedWordLine(0.5, "World"),
    new RedactedWordLine(0.5, "Data"),
    new WordLine("Hello"),
    new RedactedWordLine(0.5, "Redacted")
  )
  val wordLines44 = Seq(
    new RedactedWordLine(0.5, "World"),
    new RedactedWordLine(0.5, "Data"),
    new RedactedWordLine(0.5, "Redacted"),
    new WordLine("Hello")
  )

  val wordLines5 = Seq()

  val wordLineProjector: Projector[WordLine] = new Projector(LineConverter)
  val redactedWordLineProjector1: Projector[RedactedWordLine] = new Projector(RedactedLineConverter)
  val redactedWordLineProjector2: Projector[RedactedWordLine] = new Projector(LineConverter)

  val helloSlide3: HelloSlide[WordLine] = new HelloSlide(wordLines31)
  val helloSlide2: HelloSlide[WordLine] = new HelloSlide(wordLines2)
  val helloSlide1: HelloSlide[WordLine] = new HelloSlide(wordLines1)
  val helloSlide5: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines2)
  val helloSlide6: HelloSlide[WordLine] = new HelloSlide(wordLines5)
  val helloSlide7: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines5)
  val helloSlide51: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines22)

  "HelloSlide" should "correctly detect is it Seq(WordLine) or is it Seq(RedactedWordLine)" in {
    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines31)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines31)" should compile
    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines32)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines32)" should compile
    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines33)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines33)" should compile
    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines34)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines34)" should compile

    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines41)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines41)" should compile
    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines42)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines42)" should compile
    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines43)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines43)" should compile
    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines44)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines44)" should compile

    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines1)" shouldNot compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines1)" should compile

    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines2)" should compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines2)" should compile

    "val helloSlide: HelloSlide[RedactedWordLine] = new HelloSlide(wordLines5)" should compile
    "val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines5)" should compile
  }

  "RedactedWordLine projector" should "fail to project Slide[WordLine]" in {
    "val redactedWordLineProjection1 = redactedWordLineProjector1.project(helloSlide1)" shouldNot compile
    "val redactedWordLineProjection1 = redactedWordLineProjector1.project(helloSlide2)" shouldNot compile
    "val redactedWordLineProjection1 = redactedWordLineProjector1.project(helloSlide3)" shouldNot compile
    "val redactedWordLineProjection1 = redactedWordLineProjector1.project(helloSlide6)" shouldNot compile

    "val redactedWordLineProjection2 = redactedWordLineProjector2.project(helloSlide1)" shouldNot compile
    "val redactedWordLineProjection2 = redactedWordLineProjector2.project(helloSlide2)" shouldNot compile
    "val redactedWordLineProjection2 = redactedWordLineProjector2.project(helloSlide3)" shouldNot compile
    "val redactedWordLineProjection2 = redactedWordLineProjector2.project(helloSlide6)" shouldNot compile
  }

  "RedactedWordLine projector" should "be able to project Slide[RedactedWordLine]" in {
    "val redactedWordLineProjection1 = redactedWordLineProjector1.project(helloSlide5)" should compile
    "val redactedWordLineProjection1 = redactedWordLineProjector1.project(helloSlide7)" should compile

    "val redactedWordLineProjection2 = redactedWordLineProjector2.project(helloSlide5)" should compile
    "val redactedWordLineProjection2 = redactedWordLineProjector2.project(helloSlide7)" should compile
  }

  "WordLine projector" should "be able project Slide[WordLine]" in {
    "val wordLineProjection1 = wordLineProjector.project(helloSlide1)" should compile
    "val wordLineProjection1 = wordLineProjector.project(helloSlide2)" should compile
    "val wordLineProjection1 = wordLineProjector.project(helloSlide3)" should compile
    "val wordLineProjection1 = wordLineProjector.project(helloSlide6)" should compile
  }

  "WordLine projector" should "be able to project Slide[RedactedWordLine]" in {
    "val wordLineProjection1 = wordLineProjector.project(helloSlide5)" should compile
    "val wordLineProjection1 = wordLineProjector.project(helloSlide7)" should compile
  }

  "RedactedWordLine projector" should "redact line with given probability" in {
    import scala.util.Random

    val probability: Double = Random.nextInt(101) / 100.0
    val length: Int = Random.nextInt(10) + 1
    val redactedLine: String = "█" * length + "\n"
    val line: String = "A" * length

    val tempHelloSlide: HelloSlide[RedactedWordLine] = new HelloSlide[RedactedWordLine](Seq {
      new RedactedWordLine(probability, line)
    })
    val projector = new Projector(RedactedLineConverter)

    @tailrec
    def f(i: Int, count: Int): Double = {
      if (i == 0) count
      else {
        val result: String = projector.project(tempHelloSlide)
        if (result == redactedLine) f(i - 1, count + 1)
        else f(i - 1, count)
      }
    }

    (math.round(f(100000, 0) / 1000) / 100.0) shouldEqual probability
  }

  "WordLine projector" should "fail to use Converter[RedactedWordLine]" in {
    "val wordLineProjector1: Projector[WordLine] = new Projector(RedactedLineConverter)" shouldNot compile
  }

  "WordLine projector" should "be able to use Converter[WordLine]" in {
    "val wordLineProjector1: Projector[WordLine] = new Projector(LineConverter)" should compile
  }

  "RedactedWordLine projector" should "be able to use Converter[RedactedWordLine]" in {
    "val redactedWordLineProjector1: Projector[RedactedWordLine]  = new Projector(RedactedLineConverter)" should compile
  }

  "RedactedWordLine projector" should "be able to use Converter[WordLine]" in {
    "val redactedWordLineProjector1: Projector[RedactedWordLine] = new Projector(LineConverter)" should compile
  }

  val redactedWordLineProjection11: String = redactedWordLineProjector1.project(helloSlide51)
  val redactedWordLineProjection12: String = redactedWordLineProjector2.project(helloSlide5)

  val wordLineProjection1: String = wordLineProjector.project(helloSlide3) // HelloSlide[WordLine]
  val wordLineProjection2: String = wordLineProjector.project(helloSlide5) // HelloSlide[RedactedWordLine]

  val expectedValues: Set[String] = Set(
    "████████\n█████\n█████\n",
    "████████\n█████\nWorld\n",
    "████████\nHello\n█████\n",
    "████████\nHello\nWorld\n",
    "Redacted\n█████\n█████\n",
    "Redacted\n█████\nWorld\n",
    "Redacted\nHello\n█████\n",
    "Redacted\nHello\nWorld\n"
  )

  "All variations of projector" should "project correctly" in {
    expectedValues.contains(redactedWordLineProjection11) shouldEqual true // вероятность проверялась в другом тесте
    redactedWordLineProjection12 shouldEqual "Redacted\nHello\nWorld\nData\nAaa\n"
    wordLineProjection1 shouldEqual "Redacted\nHello\nWorld\nData\n"
    wordLineProjection2 shouldEqual "Redacted\nHello\nWorld\nData\nAaa\n"

  }
}

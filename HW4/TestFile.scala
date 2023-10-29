object TestFile extends App {
  val wordLines = Seq(
    new RedactedWordLine(0.5, "Sensitive"),
    new WordLine("Hello"),
    new WordLine("World"),
    new WordLine("Data")
  )
  val wordLines1 = Seq(
    new RedactedWordLine(0.5, "Sensitive")
  )

  val helloSlide: HelloSlide[WordLine] = new HelloSlide(wordLines)
  val helloSlide1: Slide[RedactedWordLine] = new HelloSlide(wordLines1)
  val wordLineProjector = new Projector(LineConverter)
  val redactedWordLineProjector = new Projector(RedactedLineConverter)

  // Проекция для WordLine
  val wordLineProjection1 = wordLineProjector.project(helloSlide)
  println("WordLine Projection:")
  println(wordLineProjection1)
  val wordLineProjection2 = wordLineProjector.project(helloSlide1)
  println("WordLine Projection:")
  println(wordLineProjection2)

  // Проекция для RedactedWordLine
  // val redactedWordLineProjection1 = redactedWordLineProjector.project(helloSlide)
  // println("\nRedactedWordLine Projection:")
  // println(redactedWordLineProjection1)
  val redactedWordLineProjection2 = redactedWordLineProjector.project(helloSlide)
  println("\nRedactedWordLine Projection:")
  println(redactedWordLineProjection2)
}

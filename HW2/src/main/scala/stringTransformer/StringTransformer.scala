package stringTransformer

object StringTransformer {

  def duplicate(str: String): String = str + str

  def halve(str: String): String = str.substring(0, str.length / 2)

  def reverse(str: String): String = str.reverse

  def transform(str: String)(transformFunc: String => String): String = transformFunc(str)
}

package stringTransformer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import stringTransformer.StringTransformer._

class StringTransformerSpec extends AnyFlatSpec with Matchers {
  "duplicate" should "return duplicate string" in {
    val expected = "КФУКФУ"
    duplicate("КФУ") shouldEqual expected
  }

  "halve" should "return half of string" in {
    val expected = "КФУ"
    halve("КФУУКФУ") shouldEqual expected
  }
  "halve" should "result length of odd string half is (length - 1)/2" in {
    val expected = "КФУ"
    halve("КФУУКФУ") shouldEqual expected
  }
  "reverse" should "return reverted string" in {
    val expected = "ДГВБА"
    reverse("АБВГД") shouldEqual expected
  }
  "transform" should "return result of function" in {
    val expected = "АБВАБВ"

    val result1 = transform("ВБАВБА")(reverse)
    val result2 = transform("АБВ")(duplicate)
    val result3 = transform("АБВАБВАБВАБВ")(halve)

    result1 shouldEqual expected
    result2 shouldEqual expected
    result3 shouldEqual expected
  }
}

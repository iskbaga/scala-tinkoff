package complex
import complex.ComplexNumberSyntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComplexNumberSpec extends AnyWordSpec with Matchers {
  "ComplexNumber" should {
    "sum numeric + ComplexNumber" in {
      val z = 3 - 12.0.i
      val x: ComplexNumber = 2 + z
      val y = 3.0
      val z2 = y - 12.0.i
      z shouldEqual ComplexNumber(3.0, -12.0)
      x shouldEqual ComplexNumber(5.0, -12.0)
      z2 shouldEqual ComplexNumber(3.0, -12.0)
    }
  }

}

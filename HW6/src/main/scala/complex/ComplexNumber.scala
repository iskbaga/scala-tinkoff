package complex

import complex.ComplexNumberSyntax._

// DO NOT CHANGE ANYTHING BELOW
final case class ComplexNumber(real: Double, imaginary: Double) {
  def *(other: ComplexNumber) =
    ComplexNumber(
      (real * other.real) + (imaginary * other.imaginary),
      (real * other.imaginary) + (imaginary * other.real)
    )
  def +(other: ComplexNumber) =
    ComplexNumber(real + other.real, imaginary + other.imaginary)
  def ~=(o: ComplexNumber) =
    (real - o.real).abs < 1e-6 && (imaginary - o.imaginary).abs < 1e-6
}

object ComplexNumber
// DO NOT CHANGE ANYTHING ABOVE
object ComplexNumberSyntax {
  implicit class ComplexNumberOps(num: ComplexNumber) {
    def -(other: ComplexNumber): ComplexNumber =
      ComplexNumber(num.real - other.real, num.imaginary - other.imaginary)

    def /(other: ComplexNumber): ComplexNumber = {
      val d = other.real * other.real + other.imaginary * other.imaginary
      if (d == 0) throw new ArithmeticException("Division by zero")
      ComplexNumber(
        (num.real * other.real + num.imaginary * other.imaginary) / d,
        (num.imaginary * other.real - num.real * other.imaginary) / d
      )
    }

    def toPolarForm: (Double, Double) =
      (math.sqrt(num.real * num.real + num.imaginary * num.imaginary), math.atan2(num.imaginary, num.real))
  }

  implicit class ExtendedNumeric[T](x: T)(implicit converter: Numeric[T]) {

    def convert(implicit converter: Numeric[T]): ComplexNumber =
      ComplexNumber(converter.toDouble(x), 0)
    def i: ComplexNumber = ComplexNumber(0, converter.toDouble(x))

    def +(num: ComplexNumber): ComplexNumber = convert + num

    def -(num: ComplexNumber): ComplexNumber = convert - num

    def *(num: ComplexNumber): ComplexNumber = convert * num

    def /(num: ComplexNumber): ComplexNumber = convert / num

    def ~=(num: ComplexNumber): Boolean = convert ~= num

  }

}

object Main extends App {
  val z = 3.0
  val z1 = 3 - 12.0.i
  val z2 = z - 12.0.i
  val z3 = 2 + z1
  val z4 = 2 / z1
  println(z1.toPolarForm)
  println(z1 ~= z1)
  println(z3 + z1)
  println(z + z1)
  println(z / z1)
  println(3 ~= z1)
  println(z1 ~= z2)

}

package digits

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import java.math.BigInteger

class DigitsPropertySpec extends Properties("Digits.calculate") {
  property("integer") = forAll { (n: Int) =>
    Digits.calculate(n) == calculateWithString(n)
  }

  property("long") = forAll { (n: Long) =>
    Digits.calculate(n) == calculateWithString(n)
  }

  property("short") = forAll { (n: Short) =>
    Digits.calculate(n) == calculateWithString(n)
  }

  property("byte") = forAll { (n: Byte) =>
    Digits.calculate(n) == calculateWithString(n)
  }

  property("float") = forAll { (n: Float) =>
    Digits.calculate(n) == calculateWithString(n)
  }

  property("double") = forAll { (n: Double) =>
    Digits.calculate(n) == calculateWithString(n)
  }

  def calculateWithString(number: Number): Int = {
    val stringRepresentation = number.toString

    val integerPart = stringRepresentation
      .dropWhile(negativeSign)
      .takeWhile(notDot)

    val integerDigits = integerPart.length

    if (existsE(stringRepresentation)) {
      val remaining =
        stringRepresentation
          .drop(integerDigits)
          .dropWhile(c => !equalsE(c))
          .drop(1)
          .toInt

      if (remaining > 0) integerDigits + remaining else 1
    } else integerDigits
  }

  val negativeSign: Char => Boolean = _ == '-'
  val notDot: Char => Boolean = _ != '.'
  val equalsE: Char => Boolean = _ == 'E'
  val existsE: String => Boolean = _ exists equalsE

}

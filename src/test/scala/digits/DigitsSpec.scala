package digits

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DigitsSpec extends AnyWordSpec with Matchers {
  "Digits" should {
    "calculate the number of the digits of the integer part of the number" when {
      "the input is a one digit integer" in {
        Digits.calculate(0) shouldEqual 1
      }
      "the input is the max value of integer" in {
        Digits.calculate(Int.MaxValue) shouldEqual 10
      }
      "the input is the max value of short" in {
        Digits.calculate(Short.MaxValue) shouldEqual 5
      }
      "the input is the max value of byte" in {
        Digits.calculate(Byte.MaxValue) shouldEqual 3
      }
      "the input is the max value of long" in {
        Digits.calculate(Long.MaxValue) shouldEqual 19
      }
      "the input is a negativ integer" in {
        Digits.calculate(-1) shouldEqual 1
      }
      "the input is 'bigger' than the max value of long" in {
        Digits.calculate(Long.MaxValue + 1) shouldEqual 19
      }
      "the input is a double with 2 digits precision" in {
        Digits.calculate(10.23d) shouldEqual 2
      }
      "the input is a big double" in {
        Digits.calculate(10000000000000000d) shouldEqual 17
      }
      "the input is a double with more digits before the dot and contains and E and negative" in {
        Digits.calculate(-134.123e16) shouldEqual 19
      }
      "the input is a float with 2 digits precision" in {
        Digits.calculate(10.23f) shouldEqual 2
      }
      "the input is a fmax value of float" in {
        Digits.calculate(Float.MaxValue) shouldEqual 39
      }
    }
  }
}
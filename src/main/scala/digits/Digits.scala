package digits

object Digits {

  def calculate(number: Number): Int = {
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

      integerDigits + remaining
    } else integerDigits
  }

  val negativeSign: Char => Boolean = _ == '-'
  val notDot: Char => Boolean = _ != '.'
  val equalsE: Char => Boolean = _ == 'E'
  val existsE: String => Boolean = _ exists equalsE
}

package digits

import BigDecimalConverter._

object Digits {

  def calculate[T: BigDecimalConverter](number: T): Int = {
    loop(number.toBigDecimal.abs, 0)
  }

  @scala.annotation.tailrec
  def loop(n: BigDecimal, digits: Int): Int =
    if (n < 10) digits + 1
    else loop(n / 10, digits + 1)
}

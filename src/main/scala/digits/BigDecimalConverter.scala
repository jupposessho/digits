package digits

import java.math.BigInteger
import java.util.concurrent.atomic.AtomicInteger

trait BigDecimalConverter[T] {
  def convert(t: T): BigDecimal
}

object BigDecimalConverter {
  def apply[T](implicit T: BigDecimalConverter[T]): BigDecimalConverter[T] = T

  implicit class BigDecimalConverterOps[T](t: T) {
    def toBigDecimal(implicit ev: BigDecimalConverter[T]): BigDecimal =
      ev.convert(t)
  }

  implicit val intConverter = new BigDecimalConverter[Int] {
    override def convert(t: Int): BigDecimal = BigDecimal(t)
  }
  implicit val longConverter = new BigDecimalConverter[Long] {
    override def convert(t: Long): BigDecimal = BigDecimal(t)
  }

  implicit val byteConverter = new BigDecimalConverter[Byte] {
    override def convert(t: Byte): BigDecimal = BigDecimal(t)
  }

  implicit val shortConverter = new BigDecimalConverter[Short] {
    override def convert(t: Short): BigDecimal = BigDecimal(t)
  }

  implicit val floatConverter = new BigDecimalConverter[Float] {
    override def convert(t: Float): BigDecimal = BigDecimal(t)
  }

  implicit val doubleConverter = new BigDecimalConverter[Double] {
    override def convert(t: Double): BigDecimal = BigDecimal(t)
  }

  implicit val bigDecimalConverter = new BigDecimalConverter[BigDecimal] {
    override def convert(t: BigDecimal): BigDecimal = t
  }

  implicit val bigIntegerConverter = new BigDecimalConverter[BigInteger] {
    override def convert(t: BigInteger): BigDecimal = BigDecimal(t)
  }

  implicit val bigAtomicIntegerConverter =
    new BigDecimalConverter[AtomicInteger] {
      override def convert(t: AtomicInteger): BigDecimal = BigDecimal(t.get)
    }
}

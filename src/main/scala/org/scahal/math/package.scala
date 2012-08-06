package org.scahal.math

import scala.math._

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 14/04/2012
 * Time: 23:30
 * To change this template use File | Settings | File Templates.
 */

object `package`{

  // Generic type T causes StackOverFlow, therefore implicit conversion for each numeric type
  implicit def intToBigDecimal(value: Int): BigDecimal = new java.math.BigDecimal(value.toString)

  implicit def floatToBigDecimal(value: Float): BigDecimal = new java.math.BigDecimal(value.toString)

  implicit def doubleToBigDecimal(value: Double): BigDecimal = new java.math.BigDecimal(value.toString)

  implicit def longToBigDecimal(value: Long): BigDecimal = new java.math.BigDecimal(value.toString)

  implicit def stringToBigDecimal(value: String): BigDecimal = new java.math.BigDecimal(value)

  implicit def stringListToBigDecimal(values: List[String]): List[BigDecimal] = values.map(dec(_))

  implicit def longListToBigDecimal(values: List[Long]): List[BigDecimal] = values.map(dec(_))

  implicit def doubleListToBigDecimal(values: List[Double]): List[BigDecimal] = values.map(dec(_))

  implicit def floatListToBigDecimal(values: List[Float]): List[BigDecimal] = values.map(dec(_))

  implicit def intListToBigDecimal(values: List[Int]): List[BigDecimal] = values.map(dec(_))

  /**
   * Function to force decimal implicit conversion.
   * @param decimal
   * @return
   */
  def dec(decimal: BigDecimal) = decimal

  def euclidianDist(pairs: Seq[(BigDecimal, BigDecimal)]): BigDecimal =
    dec(sqrt(pairs.foldLeft(dec(0))((input, pair) => input + dec(pow(pair._1.doubleValue() - pair._2.doubleValue(), 2))).doubleValue()))
}

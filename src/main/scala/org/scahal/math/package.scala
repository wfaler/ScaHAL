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
  implicit def intToDouble(value: Int): Double = value.toDouble

  implicit def floatToDouble(value: Float): Double = value.toDouble

  implicit def longToDouble(value: Long): Double = value.toDouble

  implicit def stringToDouble(value: String): Double = new java.math.BigDecimal(value).doubleValue()

  implicit def stringListToDouble(values: List[String]): List[Double] = values.map(dec(_))

  implicit def longListToDouble(values: List[Long]): List[Double] = values.map(dec(_))

  implicit def floatListToDouble(values: List[Float]): List[Double] = values.map(dec(_))

  implicit def intListToDouble(values: List[Int]): List[Double] = values.map(dec(_))

  /**
   * Function to force decimal implicit conversion.
   * @param decimal
   * @return
   */
  def dec(decimal: Double) = decimal

  def euclidianDist(pairs: Seq[(Double, Double)]): Double =
    dec(sqrt(pairs.foldLeft(dec(0))((input, pair) => input + pow(pair._1 - pair._2, 2))))
}

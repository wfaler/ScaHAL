package org.scahal.math.stats

import scala.math._
import org.scahal.math._

object `package`{

  def mean(values: List[Double]): Double = values.foldLeft(dec(0)){_+_} / dec(values.size)

  def median(values: List[Double]): Double = {
    val vals = values.sortWith(_>_)
    if(vals.size % 2 == 0) (vals((vals.size - 1) / 2) + vals((vals.size + 1) / 2)) / dec(2)
    else vals((vals.size - 1) / 2)
  }

  /**
   * Synonym for mean
   */
  def avg(values: List[Double]): Double = mean(values)

  def max(values: List[Double]): Double = values.max

  def min(values: List[Double]): Double = values.min

  def standardDeviation(values: List[Double]): Double = sqrt(variance(values))

  def sampleStandardDeviation(values: List[Double]): Double = sqrt(varianceWithCount(values, values.size - 1))

  def gaussian(mean: Double, stdDev: Double, value: Double): Double =
    (1/(sqrt(2*Pi)*stdDev.toDouble))*(pow(E, (-1 * pow(value.toDouble - mean.toDouble, 2)/(2*pow(stdDev.toDouble,2)))))

  //def sigmoid(z: Double): Double = dec(1.0/(1+exp(-z.toDouble)))

  def sigmoid(z: Double): Double = 1.0/(1+exp(-z))

  private def varianceWithCount(values: List[Double], count: Int): Double = {
    val average = mean(values)
    values.foldLeft(0d){(a,b) => a + pow((b - average), 2) / count.toDouble}
  }

  def variance(values: List[Double]): Double = varianceWithCount(values, values.size)

  def sharpeRatio(expectedReturn: Double, riskFree: Double, standardDeviation: Double): Double = (expectedReturn - riskFree) / standardDeviation

  def trueRange(high: Double, low: Double, prevClose: Double): Double = max(high :: prevClose :: Nil) - min(low :: prevClose :: Nil)

  /**
   * Shorthand for trueRange
   * @return
   */
  def tr(high: Double, low: Double, prevClose: Double) = trueRange(high, low, prevClose)

}

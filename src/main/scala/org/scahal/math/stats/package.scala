package org.scahal.math.stats

import scala.math._
import org.scahal.math._

object `package`{

  def mean(values: List[BigDecimal]): BigDecimal = values.foldLeft(dec(0)){_+_} / dec(values.size)

  def median(values: List[BigDecimal]): BigDecimal = {
    val vals = values.sortWith(_>_)
    if(vals.size % 2 == 0) (vals((vals.size - 1) / 2) + vals((vals.size + 1) / 2)) / dec(2)
    else vals((vals.size - 1) / 2)
  }

  /**
   * Synonym for mean
   */
  def avg(values: List[BigDecimal]): BigDecimal = mean(values)

  def max(values: List[BigDecimal]): BigDecimal = values.max

  def min(values: List[BigDecimal]): BigDecimal = values.min

  def standardDeviation(values: List[BigDecimal]): BigDecimal = sqrt(variance(values).doubleValue())

  def sampleStandardDeviation(values: List[BigDecimal]): BigDecimal = sqrt(varianceWithCount(values, values.size - 1).doubleValue())

  def gaussian(mean: BigDecimal, stdDev: BigDecimal, value: BigDecimal): BigDecimal =
    (1/(sqrt(2*Pi)*stdDev.toDouble))*(pow(E, (-1 * pow(value.toDouble - mean.toDouble, 2)/(2*pow(stdDev.toDouble,2)))))

  private def varianceWithCount(values: List[BigDecimal], count: Int): BigDecimal = {
    val average = mean(values)
    values.foldLeft(dec(0)){(a,b) => a + (b - average).pow(2) / dec(count)}
  }

  def variance(values: List[BigDecimal]): BigDecimal = varianceWithCount(values, values.size)

  def sharpeRatio(expectedReturn: BigDecimal, riskFree: BigDecimal, standardDeviation: BigDecimal): BigDecimal = (expectedReturn - riskFree) / standardDeviation

  def trueRange(high: BigDecimal, low: BigDecimal, prevClose: BigDecimal): BigDecimal = max(high :: prevClose :: Nil) - min(low :: prevClose :: Nil)

  /**
   * Shorthand for trueRange
   * @return
   */
  def tr(high: BigDecimal, low: BigDecimal, prevClose: BigDecimal) = trueRange(high, low, prevClose)

}

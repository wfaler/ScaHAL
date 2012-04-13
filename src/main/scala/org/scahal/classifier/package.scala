package org.scahal.classifier

import scala.math._
import com.recursivity.math._

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 12/04/2012
 * Time: 10:46
 * To change this template use File | Settings | File Templates.
 */

object `package`{

  def gaussian(mean: BigDecimal, stdDev: BigDecimal, value: BigDecimal): BigDecimal =
    (1/(sqrt(2*Pi)*stdDev.toDouble))*(pow(E, (-1 * pow(value.toDouble - mean.toDouble, 2)/(2*pow(stdDev.toDouble,2)))))

  def euclidianDist(pairs: Seq[(BigDecimal, BigDecimal)]): BigDecimal =
    dec(sqrt(pairs.foldLeft(dec(0))((input, pair) => input + dec(pow(pair._1.doubleValue() - pair._2.doubleValue(), 2))).doubleValue()))
}

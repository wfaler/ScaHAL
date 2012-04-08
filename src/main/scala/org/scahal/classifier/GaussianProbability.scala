package org.scahal.classifier

import scala.math._
import com.recursivity.math._

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 08/04/2012
 * Time: 16:36
 * To change this template use File | Settings | File Templates.
 */

object GaussianProbability {
  def apply(mean: BigDecimal, stdDev: BigDecimal, value: BigDecimal): BigDecimal = {
    (1/(sqrt(2*Pi)*stdDev.toDouble))*(pow(E, (-1 * pow(value.toDouble - mean.toDouble, 2)/(2*pow(stdDev.toDouble,2)))))
  }
}

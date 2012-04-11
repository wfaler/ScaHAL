package org.scahal.classifier

import com.recursivity.math._
import scala.math._

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 11/04/2012
 * Time: 16:08
 * To change this template use File | Settings | File Templates.
 */

object EuclidianDistance {

  def apply(pairs: Seq[(BigDecimal, BigDecimal)]): BigDecimal = {
    dec(sqrt(pairs.foldLeft(dec(0))((input, pair) => input + dec(pow(pair._1.doubleValue() - pair._2.doubleValue(), 2))).doubleValue()))
  }

}

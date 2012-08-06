package org.scahal.math

import org.specs2.Specification
import org.specs2.matcher.ThrownExpectations

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 14/04/2012
 * Time: 23:38
 * To change this template use File | Settings | File Templates.
 */

class PackageFunctionSpec  extends Specification with ThrownExpectations { def is =

  "The PackageFunctions implicits should"  ^
    p ^
    "convert numeric types into BigDecimal correctly" ! convert^
    "not convert a non numeric string into a BigDecimal" ! failConvert^
    "calculate the euclidian distance correctly" ! euclidianDistance^
    end


  def convert = {
    dec(1).intValue() must  be_==(new java.math.BigDecimal("1").intValue())
    dec(1f).floatValue() must  be_==(new java.math.BigDecimal("1").floatValue())
    dec(1l).longValue() must  be_==(new java.math.BigDecimal("1").longValue())
    (dec(1d) - 1).toDouble must  be_==(new java.math.BigDecimal("0.0").doubleValue())
    dec("1").toInt must  be_==(new java.math.BigDecimal("1").intValue())
  }

  def failConvert = {
    dec("bla") must  throwA[Exception]
  }

  def euclidianDistance = {
    euclidianDist(List((dec(3),dec(18)),(dec(104),dec(90)))) must be_==(dec(20.518284528683193))
  }

}

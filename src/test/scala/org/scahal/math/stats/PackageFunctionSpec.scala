package org.scahal.math.stats

import org.specs2.Specification
import java.math.BigDecimal
import org.scahal.math._

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 14/04/2012
 * Time: 23:39
 * To change this template use File | Settings | File Templates.
 */

class PackageFunctionSpec extends Specification{ def is =

  "The Stats should"  ^
    p ^
    "calculate an average correctly" ! calcAvg^
    "find the highest value" ! calcMax^
    "find the lowest value" ! calcMin^
    "calculate the standard deviation correctly" ! calcStdDev^
    "calculate the sample stabdard deviation correctly" ! calcSampleStdDev^
    "calculate the median with an uneven number of values correctly" ! calcMedianUneven^
    "calculate the median with an even number of values correctly" ! calcMedianEven^
    "calculate the median with only two values" ! calcMedianTwoValues^
    "calculate the median with only one value" ! calcMedianOneValue^
    "calculate the variance correctly" ! calcVariance^
    "calculate the ATR correctly" ! calcAtr^
    "calculate the Sharpe Ratio correctly" ! calcSharpe^
    "have a gaussian function that returns approximately 0.034 given a mean=73, stdDev=6.2 and value of 66" ! gaussianProb^
    "the sigmoid function applied to 0 should result in 0.5" ! sigmoidResult^
    end


  def calcAvg = {
    mean(List(3,5,10)) must be_==(dec(6))
  }

  def calcMax = {
    max(List(10,5,3, 15, 23, 12)) must be_==(dec(23))
  }

  def calcMin = {
    min(List(10,5,3, 15, 23, 12)) must be_==(dec(3))
  }

  def calcStdDev = {
    standardDeviation(List(2,4,4,4,5,5,7,9)) must be_==(dec(2.0d))
  }

  def calcSampleStdDev = {
    sampleStandardDeviation(List(2,4,4,4,5,5,7,9)) must be_==(dec("2.138089935299395"))
  }

  def calcMedianUneven = {
    median(List(10,5,3, 23, 12)) must be_==(dec(10))
  }

  def calcMedianEven = {
    median(List(10,5,3, 15, 23, 12)) must be_==(dec(11))
  }

  def calcMedianTwoValues = {
    median(List(5,3)) must be_==(dec(4))
  }

  def calcMedianOneValue = {
    median(List(3)) must be_==(dec(3))
  }

  def calcVariance = {
    variance(List(600, 470,170,430,300)) must be_==(dec("21704.000000000004"))
  }

  def calcSharpe = {
    sharpeRatio("0.1", "0.035", "0.16") must be_==(dec("0.40625"))
  }

  def calcAtr = {
    tr(48.7d, 47.79, 48.16d) must be_==(dec("0.9100000000000037"))
  }

  def gaussianProb = {
    gaussian(73, 6.2, 66) must be_==(dec(0.03401870545760999))
  }

  def sigmoidResult = {
    sigmoid(0) must be_==(dec(0.5))
  }

}

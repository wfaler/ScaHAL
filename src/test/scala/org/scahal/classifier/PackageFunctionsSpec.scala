package org.scahal.classifier

import org.specs2.Specification
import com.recursivity.math._


/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 08/04/2012
 * Time: 16:40
 * To change this template use File | Settings | File Templates.
 */

class PackageFunctionsSpec extends Specification{ def is =

  "The Package spec should" ^
    p^
    "have a gaussian function that returns approximately 0.034 given a mean=73, stdDev=6.2 and value of 66" ! gaussianProb^
    "calculate the euclidian distance correctly" ! euclidianDistance^
  end

  def gaussianProb = {
    gaussian(73, 6.2, 66) must be_==(dec(0.03401870545760999))
  }

  def euclidianDistance = {
    euclidianDist(List((dec(3),dec(18)),(dec(104),dec(90)))) must be_==(dec(20.518284528683193))
  }

}

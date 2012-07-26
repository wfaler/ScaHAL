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
    "calculate the euclidian distance correctly" ! euclidianDistance^
    end

  def euclidianDistance = {
    euclidianDist(List((dec(3),dec(18)),(dec(104),dec(90)))) must be_==(dec(20.518284528683193))
  }

}

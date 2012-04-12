package org.scahal.classifier.knn

import org.specs2.Specification
import org.scahal.classifier.ContinuousFeature

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 12/04/2012
 * Time: 10:45
 * To change this template use File | Settings | File Templates.
 */

class KNNClassifierSpec extends Specification{ def is =

  "The kNN Classifier should" ^
    p^
      "classify the ML book example correctly" ! pending^
  end

  def testMlBookExample = {
    val features = List(ContinuousFeature("kicks", 18), ContinuousFeature("kisses", 99))
  }

}

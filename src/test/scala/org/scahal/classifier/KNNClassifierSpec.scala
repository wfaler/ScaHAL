package org.scahal.classifier

import org.specs2.Specification
import io.Source
import org.scahal.math._
import org.specs2.matcher.ThrownExpectations

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 12/04/2012
 * Time: 10:45
 * To change this template use File | Settings | File Templates.
 */

class KNNClassifierSpec extends Specification with ThrownExpectations { def is =

  "The kNN Classifier should" ^
    p^
      "classify numerical features correctly" ! movieNumericsOnly^
      "classify categorical features correctly" ! categoricalOnly^
      "deal with missing and additional features correctly" ! missingAdditional^
  end

  def movieNumericsOnly = {
    val features = List(ContinuousFeature("kicks", 18), ContinuousFeature("kisses", 90))
    val classifier = KNNClassifier(trainKnn)
    val outcomes = classifier(features, 4)
    outcomes(0) must be_==(Outcome("romance", 0.75))
    outcomes(1) must be_==(Outcome("action", 0.25))
  }

  def categoricalOnly = {
    val training = List(Event("success", List(CategoricalFeature("1", "1"),CategoricalFeature("2", "2"),CategoricalFeature("3", "3"))),
      Event("success", List(CategoricalFeature("1", "2"),CategoricalFeature("2", "2"),CategoricalFeature("3", "3"))),
      Event("failure", List(CategoricalFeature("1", "1"),CategoricalFeature("2", "4"),CategoricalFeature("3", "5"))),
      Event("success", List(CategoricalFeature("1", "5"),CategoricalFeature("2", "1"),CategoricalFeature("3", "0"))))
    val classifier = KNNClassifier(training)

    val features = List(CategoricalFeature("1", "1"), CategoricalFeature("2", "2"), CategoricalFeature("2", "3"))
    val outcomes = classifier(features, 3)
    outcomes(0).value must be_==("success")
    outcomes(1).value must be_==("failure")
    outcomes(0).confidence.toString.substring(0,4) must be_==("0.66")
    outcomes(1).confidence.toString.substring(0,4) must be_==("0.33")
  }

  def missingAdditional = {
    val training = List(Event("success", List(CategoricalFeature("1", "1"),CategoricalFeature("2", "2"),CategoricalFeature("3", "3"))),
      Event("success", List(CategoricalFeature("1", "2"),CategoricalFeature("2", "2"),CategoricalFeature("3", "3"))),
      Event("failure", List(CategoricalFeature("1", "1"),CategoricalFeature("2", "4"),CategoricalFeature("3", "5"))),
      Event("failure", List(CategoricalFeature("1", "1"),CategoricalFeature("2", "4"),CategoricalFeature("3", "5"))),
      Event("success", List(CategoricalFeature("1", "5"),CategoricalFeature("2", "1"),CategoricalFeature("3", "0"))))
    val classifier = KNNClassifier(training)

    val features = List(CategoricalFeature("1", "1"))
    val outcomes = classifier(features, 3)
    outcomes(0).value must be_==("failure")
    outcomes(1).value must be_==("success")
    outcomes(0).confidence.toString.substring(0,4) must be_==("0.66")
    outcomes(1).confidence.toString.substring(0,4) must be_==("0.33")
  }


  def trainKnn = Source.fromInputStream(this.getClass.getResourceAsStream("/kNNMovies.txt"), "UTF-8").getLines().map(line => {
            val splits = line.split(",")
            Event(splits(3), List(ContinuousFeature("kicks", splits(1)), ContinuousFeature("kisses", splits(2))))
          }).toList

}

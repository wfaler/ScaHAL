package org.scahal.classifier.knn

import org.specs2.Specification
import io.Source
import com.recursivity.math._
import org.scahal.classifier.{Outcome, Event, ContinuousFeature}

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
      "classify numerical features correctly" ! movieNumericsOnly^
  end

  def movieNumericsOnly = {
    val features = List(ContinuousFeature("kicks", 18), ContinuousFeature("kisses", 90))
    val classifier = KNNClassifier(trainKnn)

    val outcomes = classifier(features, 4)
    (outcomes(0) must be_==(Outcome("romance", 0.75))) and
    (outcomes(1) must be_==(Outcome("action", 0.25)))

  }


  def trainKnn = Source.fromInputStream(this.getClass.getResourceAsStream("/kNNMovies.txt"), "UTF-8").getLines().map(line => {
            val splits = line.split(",")
            Event(splits(3), List(ContinuousFeature("kicks", splits(1)), ContinuousFeature("kisses", splits(2))))
          }).toList

}

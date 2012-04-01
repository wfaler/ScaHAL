package org.scahal.classifier.bayes

import io.Source
import org.scahal.classifier.{CategoricalFeature, Event}
import org.specs2.Specification

import com.recursivity.math._

/**
 * http://www.slideshare.net/aorriols/lecture10-nave-bayes
 */

class NaiveBayesClassifierSpec extends Specification { def is =

  "The FeatureMatrix should" ^
    p^
      "Add columns and rows as expected" ! happyPath^
      end

  def happyPath = {
    val events = Source.fromInputStream(this.getClass.getResourceAsStream("/tennis.txt"), "UTF-8").getLines().map(line => {
      val splits = line.split(",")
      Event(splits(4), List(CategoricalFeature("outlook", splits(0)), CategoricalFeature("temperature", splits(1)),
        CategoricalFeature("humidity", splits(2)), CategoricalFeature("wind", splits(3))))
    }).toList

    val toClassify = List(CategoricalFeature("outlook", "sunny"), CategoricalFeature("temperature", "cool"),
      CategoricalFeature("humidity", "high"),CategoricalFeature("wind", "strong"))

    val function = NaiveBayesClassifier(events)

    val outcomes = function(toClassify)

    (outcomes(0).label must be_==("no")) and
    (outcomes(0).confidence must be_>(dec(0.6))) and
    (outcomes(1).label must be_==("yes")) and
    (outcomes(1).confidence must be_<(dec(0.4)))
  }
}

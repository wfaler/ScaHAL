package org.scahal.classifier.bayes

import io.Source
import org.specs2.Specification

import com.recursivity.math._
import org.scahal.classifier.{BagOfWordsFeature, CategoricalFeature, Event}

/**
 * http://www.slideshare.net/aorriols/lecture10-nave-bayes
 */

class NaiveBayesClassifierSpec extends Specification { def is =

  "The Naive Bayesian Classifier should" ^
    p^
      "Classify correctly given a simple categorical feature set" ! happyPath^
      "Throw an exception if probabilities do not contain all labels in training data" ! failProbsValidation^
      "Throw an exception if training data does not contain all probability labels" ! failProbsTrainingData^
      "Smooth out feature values to avoid zero-division" ! zeroDivisionSmoothing^
      "Smooth out lacking feature categories to avoid zero division" ! bagOfWords^
      "Be able to work with continuous (numeric) features" ! pending^
      end

  def happyPath = {
    val toClassify = List(CategoricalFeature("outlook", "sunny"), CategoricalFeature("temperature", "cool"),
      CategoricalFeature("humidity", "high"),CategoricalFeature("wind", "strong"))

    val function = NaiveBayesClassifier(events)

    val outcomes = function(toClassify)
    (outcomes(0).label must be_==("no")) and
    (outcomes(0).confidence must be_>(dec(0.82))) and
    (outcomes(1).label must be_==("yes")) and
    (outcomes(1).confidence must be_<(dec(0.18)))
  }

  def failProbsValidation = {
    NaiveBayesClassifier(events, Some(Map("yes" -> dec(1)))) must throwAn[IllegalStateException]
  }

  def failProbsTrainingData = {
    NaiveBayesClassifier(events, Some(Map("yes" -> dec(0.5), "no" -> dec(0.25),"maybe" -> dec(0.25)))) must throwAn[IllegalStateException]
  }

  def zeroDivisionSmoothing = {
    val toClassify = List(CategoricalFeature("outlook", "overcast"), CategoricalFeature("temperature", "cool"),
      CategoricalFeature("humidity", "high"),CategoricalFeature("wind", "strong"))

    val function = NaiveBayesClassifier(events)

    val outcomes = function(toClassify)
    println(outcomes)

    outcomes must have size(2)
  }

  def bagOfWords = {
    val evs = List(Event("spam", List(BagOfWordsFeature("viagra"), BagOfWordsFeature("cialis"), BagOfWordsFeature("president"))),
      Event("ham", List(BagOfWordsFeature("puppies"), BagOfWordsFeature("cats"), BagOfWordsFeature("cars"))))

    val function = NaiveBayesClassifier(evs)
    val toClassify = List(BagOfWordsFeature("spam"), BagOfWordsFeature("puppies"), BagOfWordsFeature("cats"), BagOfWordsFeature("unspecified"))

    val outcomes = function(toClassify)
    println(outcomes)

    outcomes must have size(2)
  }

  def events = Source.fromInputStream(this.getClass.getResourceAsStream("/tennis.txt"), "UTF-8").getLines().map(line => {
        val splits = line.split(",")
        Event(splits(4), List(CategoricalFeature("outlook", splits(0)), CategoricalFeature("temperature", splits(1)),
          CategoricalFeature("humidity", splits(2)), CategoricalFeature("wind", splits(3))))
      }).toList
}

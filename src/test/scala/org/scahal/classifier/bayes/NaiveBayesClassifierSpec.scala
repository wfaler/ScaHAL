package org.scahal.classifier.bayes

import io.Source
import org.specs2.Specification

import com.recursivity.math._
import org.scahal.classifier._

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
      "Give correct probabilities when no features are given" ! noFeatures^
      "Be able to work with continuous (numeric) features only " ! numericOnly^
      "Throw an Exception if a numeric feature is missing" ! missingNumeric^
      "Be able to work with mixed continuous and categorical features " ! mixedFeatures^
      end

  def happyPath = {
    val toClassify = List(CategoricalFeature("outlook", "sunny"), CategoricalFeature("temperature", "cool"),
      CategoricalFeature("humidity", "high"),CategoricalFeature("wind", "strong"))

    val function = NaiveBayesClassifier(events)

    val outcomes = function(toClassify)
    (outcomes(0).value must be_==("no")) and
    (outcomes(0).confidence must be_>(dec(0.73))) and
    (outcomes(1).value must be_==("yes")) and
    (outcomes(1).confidence must be_<(dec(0.27)))
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

    (outcomes must have size(2)) and
    (outcomes(0).value must be_==("yes")) and
    (outcomes(0).confidence must be_>(dec(0.7))) and
    (outcomes(1).value must be_==("no")) and
    (outcomes(1).confidence must be_<(dec(0.3)))
  }

  def bagOfWords = {
    val evs = List(Event("spam", List(BagOfWordsFeature("viagra"), BagOfWordsFeature("cialis"), BagOfWordsFeature("president"))),
      Event("ham", List(BagOfWordsFeature("puppies"), BagOfWordsFeature("cats"), BagOfWordsFeature("cars"))))

    val function = NaiveBayesClassifier(evs)
    val toClassify = List(BagOfWordsFeature("spam"), BagOfWordsFeature("puppies"), BagOfWordsFeature("cats"), BagOfWordsFeature("unspecified"))

    val outcomes = function(toClassify)
    (outcomes must have size(2)) and
    (outcomes(0).value must be_==("ham")) and
    (outcomes(0).confidence must be_>(dec(0.8))) and
    (outcomes(1).value must be_==("spam")) and
    (outcomes(1).confidence must be_<(dec(0.2)))
  }

  def noFeatures = {
    val function = NaiveBayesClassifier(events)
    val outcomes = function(Seq[Feature]())
    (outcomes must have size(2)) and
    (outcomes(0).value must be_==("yes")) and
    (outcomes(0).confidence must be_>(dec(0.64))) and
    (outcomes(1).value must be_==("no")) and
    (outcomes(1).confidence must be_<(dec(0.36)))
  }

  def numericOnly = {
    val function = NaiveBayesClassifier(numerical)
    val outcomes = function(List(ContinuousFeature("height", 6), ContinuousFeature("weight", 130), ContinuousFeature("shoes", 8)))

    (outcomes must have size(2)) and
    (outcomes(0).value must be_==("female"))
  }

  def missingNumeric = {
    val function = NaiveBayesClassifier(numerical)
    function(List(ContinuousFeature("height", 6), ContinuousFeature("weight", 130))) must throwAn[IllegalStateException]
  }

  def mixedFeatures = {
    val function = NaiveBayesClassifier(mixed)
    val outcomes = function(List(ContinuousFeature("height", 6), ContinuousFeature("weight", 130), ContinuousFeature("shoes", 8),
      CategoricalFeature("bag", "no")))

    val nonMixed = NaiveBayesClassifier(numerical)(List(ContinuousFeature("height", 6), ContinuousFeature("weight", 130), ContinuousFeature("shoes", 8)))

    (outcomes must have size(2)) and
    (outcomes(0).value must be_==("female")) and
    (nonMixed must have size(2)) and
    (nonMixed(0).value must be_==("female")) and
    ((nonMixed(0).confidence) must be_>=(outcomes(0).confidence))
  }

  def numerical = Source.fromInputStream(this.getClass.getResourceAsStream("/numerical.txt"), "UTF-8").getLines().map(line => {
          val splits = line.split(",")
          Event(splits(0), List(ContinuousFeature("height", splits(1)), ContinuousFeature("weight", splits(2)),
            ContinuousFeature("shoes", splits(3))))
        }).toList

  def mixed = Source.fromInputStream(this.getClass.getResourceAsStream("/numerical.txt"), "UTF-8").getLines().map(line => {
          val splits = line.split(",")
          Event(splits(0), List(ContinuousFeature("height", splits(1)), ContinuousFeature("weight", splits(2)),
            ContinuousFeature("shoes", splits(3)), CategoricalFeature("bag", splits(4))))
        }).toList

  def events = Source.fromInputStream(this.getClass.getResourceAsStream("/tennis.txt"), "UTF-8").getLines().map(line => {
        val splits = line.split(",")
        Event(splits(4), List(CategoricalFeature("outlook", splits(0)), CategoricalFeature("temperature", splits(1)),
          CategoricalFeature("humidity", splits(2)), CategoricalFeature("wind", splits(3))))
      }).toList
}

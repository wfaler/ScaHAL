package org.scahal.classifier.bayes

import org.scahal.classifier.{Feature, Classifier, FeatureMatrix, Event}


/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 19:49
 * To change this template use File | Settings | File Templates.
 */

object NaiveBayesClassifier {

  def apply(model: Map[String, FeatureMatrix]): Classifier = {
    model.keys.flatMap(key => {
      model(key).columns.flatMap(featureColumn => {

      })
    })
    null
  }
}

class NaiveBayesClassifier extends Classifier{
  def classify(features: Seq[Feature]) = null
}

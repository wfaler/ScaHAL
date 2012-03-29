package org.scahal.classifier.bayes

import org.scahal.classifier._


/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 19:49
 * To change this template use File | Settings | File Templates.
 */

object NaiveBayesClassifier {
  
  def apply(events: List[Event]) = apply(ModelBuilder(events))

  def apply(model: Map[String, FeatureMatrix]): Seq[Feature] => List[Outcome] = {
    model.keys.flatMap(key => {
      model(key).columns.flatMap(featureColumn => {

      })
    })
    null
  }
}
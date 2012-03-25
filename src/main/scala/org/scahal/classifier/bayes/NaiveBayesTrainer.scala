package org.scahal.classifier.bayes

import org.scahal.classifier.{FeatureMatrix, Event}


/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 19:49
 * To change this template use File | Settings | File Templates.
 */

object NaiveBayesTrainer {

  def train(events: Seq[Event]): Map[String, FeatureMatrix] = train(Map[String, FeatureMatrix](), events)

  def train(model: Map[String, FeatureMatrix], events: Seq[Event]): Map[String, FeatureMatrix] = events.foldLeft(model){train(_,_)}
  
  def train(model: Map[String, FeatureMatrix], event: Event): Map[String, FeatureMatrix] = model.get(event.outcome) match{
        case None => model ++ Map(event.outcome -> FeatureMatrix(event.features))
        case Some(matrix) => (model - event.outcome) ++ Map(event.outcome -> matrix(event.features))
      }
}

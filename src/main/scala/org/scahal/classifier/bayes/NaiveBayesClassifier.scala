package org.scahal.classifier.bayes

import org.scahal.classifier._
import com.recursivity.math._


/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 19:49
 * To change this template use File | Settings | File Templates.
 */

object NaiveBayesClassifier {
  
  def apply(events: Seq[Event], outcomeProbabilities: Option[Map[String, BigDecimal]] = None): Seq[Feature] => List[Outcome] =
    train(ModelBuilder(events), outcomeProbabilities)

  def train(model: Map[String, FeatureMatrix], outcomeProbabilities: Option[Map[String, BigDecimal]] = None): Seq[Feature] => List[Outcome] = {
    val columns = model.keys.flatMap(model(_).columns).toSet
    val outcomes = model.foldLeft((Map[String, Int]()))((map, outcomeMatrix) => map + (outcomeMatrix._1 -> outcomeMatrix._2.rows.size))

    NaiveBayesClassifier(outcomes, outcomeProbs(outcomes, outcomeProbabilities), columns, featureCounts(model.keys.flatMap(key => {
          columns.flatMap(column => List((key, column, model(key).rows.flatMap(_.find(_.featureColumn == column)))))}).toList)).classify(_)
  }

  private def outcomeProbs(outcomes: Map[String, Int], outcomeProbabilities: Option[Map[String, BigDecimal]]): Map[String, BigDecimal] =
    outcomeProbabilities.map(probs => {
      probs.foreach(v => outcomes.get(v._1).getOrElse(throw new IllegalStateException("Classifier has not had any training data for outcome " + v._1)))
      outcomes.foreach(v => probs.get(v._1).getOrElse(throw new IllegalStateException("Training data has a label that is not present in outcome probabilities: " + v._1)))
      probs
    }).getOrElse(outcomes.map(tup => (tup._1 -> (dec(tup._2) / dec(outcomes.foldLeft(0)((int, tuple) => int + tuple._2))))))


  private def featureCounts(allFeatures: List[(String, FeatureColumn, Seq[Feature])]): Map[(String, FeatureColumn), Map[Feature, Int]] =
    allFeatures.foldLeft(Map[(String, FeatureColumn), Map[Feature, Int]]())((map, entry) => {
        entry._3.foldLeft(map)((map, feature) => {
          map.get((entry._1, entry._2)).map(featureMap => {
            (map.-((entry._1, entry._2))).+((entry._1, entry._2) -> (featureMap.-(feature).+(feature -> (featureMap.get(feature).getOrElse(0) + 1))))
          }).getOrElse(map + ((entry._1, entry._2) -> Map(feature -> 1)))
        })
      })

}

case class NaiveBayesClassifier(outcomes: Map[String, Int], outcomeProbabilities: Map[String, BigDecimal], columns: Set[FeatureColumn],
                                featureCount: Map[(String, FeatureColumn), Map[Feature, Int]]){
  def classify(features: Seq[Feature]): List[Outcome] = {
    val outcomeResults = outcomes.foldLeft(List[Outcome]())((results, entry) => {
      results ++ List(Outcome(entry._1, columns.foldLeft(outcomeProbabilities(entry._1))((lastResult, column) => {
        lastResult * dec(dec(featureCount((entry._1, column)).get(features.find(_.featureColumn == column).get).get) / dec(outcomes(entry._1)))
      })))
    })

    val total = outcomeResults.foldLeft(dec(0))(_+_.confidence)
    outcomeResults.map(o => Outcome(o.label, o.confidence / total)).sortWith(_.confidence>_.confidence)
  }
}
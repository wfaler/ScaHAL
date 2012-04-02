package org.scahal.classifier.bayes

import org.scahal.classifier._
import com.recursivity.math._

object NaiveBayesClassifier {
  
  def apply(events: Seq[Event], outcomeProbabilities: Option[Map[String, BigDecimal]] = None): Seq[Feature] => List[Outcome] =
    train(ModelBuilder(events), outcomeProbabilities)

  def train(model: Map[String, FeatureMatrix], outcomeProbabilities: Option[Map[String, BigDecimal]] = None): Seq[Feature] => List[Outcome] = {
    val columns = model.keys.flatMap(model(_).columns).toSet
    val outcomes = model.foldLeft((Map[String, Int]()))((map, outcomeMatrix) => map + (outcomeMatrix._1 -> outcomeMatrix._2.rows.size))
    val classifier = NaiveBayesClassifier(outcomes, outcomeProbs(outcomes, outcomeProbabilities),
      columns, featureCounts(model, LaplaceInitialMap(model)))
    classifier.classify(_)
  }

  private def outcomeProbs(outcomes: Map[String, Int], outcomeProbabilities: Option[Map[String, BigDecimal]]): Map[String, BigDecimal] = {
    outcomeProbabilities.map(probs => {
      probs.foreach(v => outcomes.get(v._1).getOrElse(throw new IllegalStateException("Classifier has not had any training data for outcome " + v._1)))
      outcomes.foreach(v => probs.get(v._1).getOrElse(throw new IllegalStateException("Training data has a label that is not present in outcome probabilities: " + v._1)))
      probs
    }).getOrElse(outcomes.map(tup => (tup._1 -> (dec(tup._2) / dec(outcomes.foldLeft(0)((int, tuple) => int + tuple._2))))))
  }


  private def featureCounts(model: Map[String, FeatureMatrix], laplaceMap: Map[String,Map[FeatureColumn,Map[org.scahal.classifier.Feature,Int]]]): Map[String,Map[FeatureColumn,Map[Feature,Int]]] = {
    laplaceMap.map(outcomeEntry => {
       (outcomeEntry._1, outcomeEntry._2.map(columnEntry => {
         val columnFeatures = model(outcomeEntry._1).rows.foldLeft(List[Feature]())((features, row) => {
           features ++ row.filter(_.featureColumn == columnEntry._1)
         })
         (columnEntry._1, columnEntry._2.map(featureEntry => {
                    (featureEntry._1, featureEntry._2 + columnFeatures.filter(_==featureEntry._1).size)
         }))
       }))
    })
  }


}

case class NaiveBayesClassifier(outcomes: Map[String, Int], outcomeProbabilities: Map[String, BigDecimal], columns: Set[FeatureColumn],
                                featureCount: Map[String,Map[FeatureColumn,Map[Feature,Int]]]){
  def classify(features: Seq[Feature]): List[Outcome] = {
    val outcomeResults = outcomes.foldLeft(List[Outcome]())((results, entry) => {
      val laplaceSmoothingCoefficient = outcomes.keys.size  // based on the number of classes
      results ++ List(Outcome(entry._1, columns.foldLeft(outcomeProbabilities(entry._1))((lastResult, column) => {
       featureCount.get((entry._1)).get.get(column).map(featuresMap => {
         featuresMap.get(features.find(_.featureColumn == column).getOrElse(NonFeature)).map(value => {
           lastResult * (dec(value) / dec(outcomes(entry._1) + laplaceSmoothingCoefficient))
         }).getOrElse(lastResult)
       }).getOrElse(lastResult)
      })))
    })
    val total = outcomeResults.foldLeft(dec(0))(_+_.confidence)
    outcomeResults.map(o => Outcome(o.label, o.confidence / total)).sortWith(_.confidence>_.confidence)
  }
}
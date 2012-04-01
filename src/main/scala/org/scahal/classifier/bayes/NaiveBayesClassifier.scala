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
  
  def apply(events: Seq[Event], outcomeProbabilities: Option[Map[String, BigDecimal]] = None): Seq[Feature] => List[Outcome] = train(ModelBuilder(events), outcomeProbabilities)

  def train(model: Map[String, FeatureMatrix], outcomeProbabilities: Option[Map[String, BigDecimal]] = None): Seq[Feature] => List[Outcome] = {
    val columns = model.keys.flatMap(model(_).columns).toSet
    val outcomes = model.foldLeft((Map[String, Int]()))((map, outcomeMatrix) => map + (outcomeMatrix._1 -> outcomeMatrix._2.rows.size))

    val allFeatures: List[(String, FeatureColumn, Seq[Feature])] = model.keys.flatMap(key => {
      columns.flatMap(column => {
        List((key, column, model(key).rows.flatMap(_.find(_.featureColumn == column))))
      })
    }).toList

    val featureCount = allFeatures.foldLeft(Map[(String, FeatureColumn), Map[Feature, Int]]())((map, entry) => {
      entry._3.foldLeft(map)((map, feature) => {
        map.get((entry._1, entry._2)) match{
          case None => map + ((entry._1, entry._2) -> Map(feature -> 1))
          case Some(featureMap) => {
            val count = featureMap.get(feature).getOrElse(0) + 1
            (map.-((entry._1, entry._2))).+((entry._1, entry._2) -> (featureMap.-(feature).+(feature -> count)))
          }
        }
      })
    })
    NaiveBayesClassifier(outcomes, outcomeProbs(outcomes, outcomeProbabilities), columns, featureCount).apply(_)
  }

  private def outcomeProbs(outcomes: Map[String, Int], outcomeProbabilities: Option[Map[String, BigDecimal]]): Map[String, BigDecimal] = {
    outcomeProbabilities match{
      case None => {
        val outcomeCount = outcomes.foldLeft(0)((int, tuple) => int + tuple._2)
        outcomes.map(tup => (tup._1 -> (dec(tup._2) / dec(outcomeCount))))
      }
      case Some(probs) => {
        null
      }
    }

  }

}

case class NaiveBayesClassifier(outcomes: Map[String, Int], outcomeProbabilities: Map[String, BigDecimal], columns: Set[FeatureColumn],
                                featureCount: Map[(String, FeatureColumn), Map[Feature, Int]]){
  def apply(features: Seq[Feature]): List[Outcome] = {
    val outcomeResults = outcomes.foldLeft(List[Outcome]())((results, entry) => {
      val divisor = dec(outcomes(entry._1))

      val probability = columns.foldLeft(outcomeProbabilities(entry._1))((lastResult, column) => {
        val featureCountMap = featureCount((entry._1, column))
        println(featureCountMap)
        val count = dec(featureCount((entry._1, column)).get(features.find(_.featureColumn == column).get).get)
        lastResult * dec(count / divisor)
      })
      results ++ List(Outcome(entry._1, probability))
    }).sortWith(_.confidence>_.confidence)

    val total = outcomeResults.foldLeft(dec(0))(_+_.confidence)
    outcomeResults.map(o => Outcome(o.label, o.confidence / total))
  }
}
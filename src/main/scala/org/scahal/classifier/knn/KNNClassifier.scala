package org.scahal.classifier.knn

import org.scahal.classifier._
import com.recursivity.math._
import scala.math._


/**
 * K Nearest Neighbour Classifier
 */

case class KNNClassifier[T](events: Seq[Event[T]]) {

  def apply(features: Seq[Feature], kSize: Int): List[Outcome[T]] = {
    val distances = events.foldLeft(List[(BigDecimal, Event[T])]())((input, event) => {
      val dist = event.features.foldLeft(dec(0))((distance, feature) => {
        feature match{
          case ContinuousFeature(name,value) => distance + pow(value.toDouble - features.
            find(_.featureColumn == feature.featureColumn).map(f => f.asInstanceOf[ContinuousFeature].value.doubleValue()).getOrElse(0d), 2d)
          case CategoricalFeature(name, value) => distance + features.find(_ == feature).map(f => dec(1)).getOrElse(dec(0))
          case _ => throw new IllegalArgumentException("kNN classifier can only deal with Continuous and Categorical Features")
        }
      })
      val totalDistance = features.filterNot(f => event.features.exists(_.featureColumn==f.featureColumn)).foldLeft(dist)((distance, feature) => {
        feature match{
          case ContinuousFeature(name,value) => distance + pow(value.toDouble, 2d)
          case CategoricalFeature(name, value) => distance + 1
          case _ => throw new IllegalArgumentException("kNN classifier can only deal with Continuous and Categorical Features")
        }
      })
      (dec(sqrt(totalDistance.doubleValue())), event) :: input
    }).sortWith(_._1 < _._1)

    List.range(0,kSize).foldLeft(Map[T, Int]())((map, index) => {
      val outcome = distances(index)._2.outcome
      map.get(outcome).map(int => {
        map.updated(outcome, int + 1)
      }).getOrElse({
        map.+((outcome, 1))
      })
    }).map(vote => Outcome(vote._1, dec(vote._2) / kSize)).toList.sortWith(_.confidence > _.confidence)
  }
}

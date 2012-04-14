package org.scahal.classifier.knn

import org.scahal.classifier._
import com.recursivity.math._
import scala.math._


/**
 * k Nearest Neighbour Classifier
 */

case class KNNClassifier[T](events: Seq[Event[T]]) {

  def apply(features: Seq[Feature], kSize: Int): List[Outcome[T]] = {
    val distances = events.foldLeft(List[(BigDecimal, Event[T])]())((input, event) => {
      val pairs = event.features.map(feature => {
        feature match{
          case ContinuousFeature(name,value) => (value, features.
            find(_.featureColumn == feature.featureColumn).map(f => f.asInstanceOf[ContinuousFeature].value).getOrElse(dec(0)))
          case CategoricalFeature(name, value) => (dec(1), features.find(_ == feature).map(f => dec(1)).getOrElse(dec(0)))
          case _ => throw new IllegalArgumentException("kNN classifier can only deal with Continuous and Categorical Features")
        }
      }) ++ features.filterNot(f => event.features.exists(_.featureColumn==f.featureColumn)).map(feature => {
        feature match{
          case ContinuousFeature(name,value) => (value, dec(0))
          case CategoricalFeature(name, value) => (dec(1), dec(0))
          case _ => throw new IllegalArgumentException("kNN classifier can only deal with Continuous and Categorical Features")
        }
      })
      (euclidianDist(pairs), event) :: input
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

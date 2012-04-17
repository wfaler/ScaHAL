package org.scahal.classifier.logistic

import org.scahal.classifier.{FeatureColumn, Event}
import org.scahal.math._
import scalala.tensor.dense.DenseVectorCol
import scalala.tensor.VectorCol

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 15/04/2012
 * Time: 17:46
 * To change this template use File | Settings | File Templates.
 */


object StochasticGradientAscent{
  def apply(events: Seq[Event[Double]], alpha: Double = 0.001): List[BigDecimal] = {
    val columns = events.foldLeft(Set[FeatureColumn]())((set, event) => event.features.map(_.featureColumn).toSet ++ set)
   // VectorCol[Double]
    val weights: VectorCol[Double] = DenseVectorCol.ones[Double](events.size)

    Range(0, events.size).foldLeft(weights)((w, counter) => {
      val h = 1d  //= sigmoid(sum(dataMatrix[i]*weights))
      val error = events(counter).outcome - h
     // weights = weights + alpha * error * dataMatrix[i]
      null
    })
    null
  }
}

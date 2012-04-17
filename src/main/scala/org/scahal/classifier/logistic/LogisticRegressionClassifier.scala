package org.scahal.classifier.logistic

import org.scahal.math.stats._
import scalala.tensor.VectorCol
import scalala.tensor.mutable._;
import scalala.tensor.dense._
import org.scahal.classifier.{ContinuousFeature, FeatureColumn, Event}
;

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 15/04/2012
 * Time: 17:46
 * To change this template use File | Settings | File Templates.
 */


object GradientAscent{

  def apply(events: Seq[Event[Double]], alpha : Double = 0.001, maxCycles: Int = 500): VectorCol[Double] = {
    val columns = events.foldLeft(Set[FeatureColumn]())((set, event) => event.features.map(_.featureColumn).toSet ++ set).toList
    val labels: VectorCol[Double] = DenseVectorCol(events.map(_.outcome).toArray)

    val trainingSet: Matrix[Double] = DenseMatrix.tabulate(events.size, columns.size)((i,j) => {
       events(i).features.find(columns(j) == _.featureColumn).map(_.asInstanceOf[ContinuousFeature].value.toDouble).getOrElse(0d)
    })

    Range(0, maxCycles).foldLeft(DenseVectorCol.ones[Double](columns.size))((weights, c) => {
      val error = labels - ((trainingSet * weights).map(sigmoid(_)))
      weights + (trainingSet.t * error) * alpha
    })
  }

}
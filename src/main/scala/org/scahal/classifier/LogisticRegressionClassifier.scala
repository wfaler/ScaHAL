package org.scahal.classifier

import org.scahal.math._
import org.scahal.math.stats._
import scalala.tensor.VectorCol

import scalala.tensor.dense._
import scalala.tensor.mutable._

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 15/04/2012
 * Time: 17:46
 * To change this template use File | Settings | File Templates.
 */

case class OptimizationAlgorithmProperties(events: Seq[Event[Double]]){
  val columns = events.foldLeft(Set[FeatureColumn]())((set, event) => event.features
    .map(_.featureColumn).toSet ++ set).toList
  
  val labels: VectorCol[Double] = DenseVectorCol(events.map(_.outcome).toArray)

  val trainingSet: Matrix[Double] =  DenseMatrix.tabulate(events.size, columns.size)((i,j) => {
             events(i).features.find(columns(j) == _.featureColumn).
               map(_.asInstanceOf[ContinuousFeature].value.toDouble).getOrElse(0d)
          })
}

/**
 * Calculates weights for the LogisticRegression classifier given a set of training data.
 * A more computationally (but potentially less accurate) approach is using Stochastic Gradient Ascent calculations,
 * which calculates ascent from a sample. Stochastic Gradient Ascent is currently not part of the ScaHAL implementation
 */
object GradientAscent{
  def apply(events: Seq[Event[Double]], alpha : Double = 0.001, maxCycles: Int = 500): List[Double] = {
    val properties = OptimizationAlgorithmProperties(events)

    Range(0, maxCycles).par.foldLeft(DenseVectorCol.ones[Double](properties.columns.size))((weights, c) => {
      val error = properties.labels - ((properties.trainingSet * weights).map(sigmoid(_)))
      weights + (properties.trainingSet.t * error) * alpha
    }).toList
  }
}


object LogisticRegressionClassifier{
  def apply(features: Seq[ContinuousFeature], weights: List[Double]): List[Outcome[Double]] = {
    val classification = Range(0, weights.size).foldLeft(0d)((in, index) =>{
          in + features(index).value.toDouble * weights(index)
        })
    if(classification > 0.5) List(Outcome(1d, dec(100)))
    else List(Outcome(0d, dec(100)))
  }
}

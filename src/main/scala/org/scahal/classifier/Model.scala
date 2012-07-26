package org.scahal.classifier

import org.scahal.math._
/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 19:28
 * To change this template use File | Settings | File Templates.
 */
case class FeatureColumn(name: String, cls: Class[ _ <: Feature])

case class Outcome[T](value: T, confidence: Double)

case class Event[T](outcome: T, features: Seq[Feature])

sealed trait Feature {
  def name: String
  def featureColumn = FeatureColumn(name, this.getClass)
}

case object ZeroFeature extends Feature{
  def name = "zeroFeature"
}

case class CategoricalFeature[T](name: String, category: T) extends Feature

object ContinuousFeature{
  def apply(name: String, value: BigDecimal): ContinuousFeature = ContinuousFeature(name, value.toDouble)
  def apply(name: String, value: Int): ContinuousFeature = ContinuousFeature(name, dec(value))
  def apply(name: String, value: Long): ContinuousFeature = ContinuousFeature(name, dec(value))
  def apply(name: String, value: Float): ContinuousFeature = ContinuousFeature(name, dec(value))
}

case class ContinuousFeature(name: String, value: Double) extends Feature

object BagOfWordsFeature{
  def apply(word: String): Feature = CategoricalFeature(word, word)
}

case class GaussianDistribution(mean: Double, standardDeviation: Double)


package org.scahal.classifier

/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 19:28
 * To change this template use File | Settings | File Templates.
 */
case class FeatureColumn(name: String, cls: Class[ _ <: Feature])

case class Outcome(label: String, confidence: BigDecimal)

case class Event(outcome: String, features: Seq[Feature])

sealed trait Feature {
  def name: String
  def featureColumn = FeatureColumn(name, this.getClass)
}

case object NonFeature extends Feature{
  def name = "NonFeature"
}

case class CategoricalFeature[T](name: String, category: T) extends Feature

case class ContinuousFeature(name: String, value: BigDecimal) extends Feature

object BagOfWordsFeature{
  def apply(word: String): Feature = CategoricalFeature(word, word)
}


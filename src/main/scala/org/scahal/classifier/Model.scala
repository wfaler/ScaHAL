package org.scahal.classifier

/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 19:28
 * To change this template use File | Settings | File Templates.
 */
trait Feature {
  def name: String
}

case class CategoricalFeature[T](name: String, category: T)

case class ContinuousFeature(name: String, value: BigDecimal)

case class BagOfWordsFeature(name: String, count: Long)


case class Outcome(label: String, probability: Double)

case class Event(outcome: String, features: Seq[Feature])



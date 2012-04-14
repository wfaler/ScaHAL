package org.scahal.classifier.bayes

import org.scahal.classifier._
import org.scahal.math._
import org.scahal.math.stats._

object NaiveBayesClassifier {
  
  def apply[T](events: Seq[Event[T]], outcomeProbabilities: Option[Map[T, BigDecimal]] = None): Seq[Feature] => List[Outcome[T]] =
    train(ModelBuilder(events), outcomeProbabilities)

  def train[T](model: Map[T, FeatureMatrix], outcomeProbabilities: Option[Map[T, BigDecimal]] = None): Seq[Feature] => List[Outcome[T]] = {
    val outcomes = model.foldLeft((Map[T, Int]()))((map, outcomeMatrix) => map + (outcomeMatrix._1 -> outcomeMatrix._2.rows.size))
    val classifier = NaiveBayesClassifier[T](outcomes, model, outcomeProbs(outcomes, outcomeProbabilities))
    classifier.classify(_)
  }

  private def outcomeProbs[T](outcomes: Map[T, Int], outcomeProbabilities: Option[Map[T, BigDecimal]]): Map[T, BigDecimal] = {
    outcomeProbabilities.map(probs => {
      probs.foreach(v => outcomes.get(v._1).getOrElse(throw new IllegalStateException("Classifier has not had any training data for outcome " + v._1)))
      outcomes.foreach(v => probs.get(v._1).getOrElse(throw new IllegalStateException("Training data has a value that is not present in outcome probabilities: " + v._1)))
      probs
    }).getOrElse(outcomes.map(tup => (tup._1 -> (dec(tup._2) / dec(outcomes.foldLeft(0)((int, tuple) => int + tuple._2))))))
  }
}

case class NaiveBayesClassifier[T](outcomes: Map[T, Int], model: Map[T, FeatureMatrix], outcomeProbabilities: Map[T, BigDecimal]){
  private val categoricalColumns = model.keys.flatMap(model(_).columns).toSet.filter(_.cls.isAssignableFrom(classOf[CategoricalFeature[_]]))
  private val continuousColumns = model.keys.flatMap(model(_).columns).toSet.filter(_.cls.isAssignableFrom(classOf[ContinuousFeature]))
  private val featureCount = featureCounts(model)
  private val continuousFeatures = gaussianModel(model)

  def classify(features: Seq[Feature]): List[Outcome[T]] = {
    val outcomeResults = outcomes.foldLeft(List[Outcome[T]]())((results, entry) => {
      val laplaceSmoothingCoefficient = outcomes.keys.size  // based on the number of classes

       Outcome(entry._1, categoricalColumns.foldLeft(outcomeProbabilities(entry._1))((lastResult, column) => {
       featureCount.get((entry._1)).get.get(column).map(featuresMap => {
         featuresMap.get(features.find(_.featureColumn == column).getOrElse(NonFeature)).map(value => {
           lastResult * (dec(value) / dec(outcomes(entry._1) + laplaceSmoothingCoefficient))
         }).getOrElse(lastResult)
       }).getOrElse(lastResult)
      })) :: results
    }).map(outcome => {
      continuousColumns.foldLeft(outcome)((input, column) => {
        val dist = continuousFeatures.get(input.value).map(_.apply(column)).getOrElse(throw new IllegalStateException("All Continuous Features must have mean and std dev values"))
        val feature = features.find(_.featureColumn == column).getOrElse(throw new IllegalStateException(column.name + "has not value in feature set, cannot evaluate data with missing continuous values"))
        val gaussianProb = gaussian(dist.mean, dist.standardDeviation, feature.asInstanceOf[ContinuousFeature].value)
        Outcome(outcome.value, outcome.confidence * gaussianProb)
      })
    })
    val total = outcomeResults.foldLeft(dec(0))(_+_.confidence)
    outcomeResults.map(o => Outcome(o.value, o.confidence / total)).sortWith(_.confidence>_.confidence)
  }

  private def gaussianModel(model: Map[T, FeatureMatrix]): Map[T,Map[FeatureColumn, GaussianDistribution]] = {
    model.map(values => {
      (values._1, values._2.continuousFeatures().map(features => {
        (features._1, GaussianDistribution(mean(features._2.map(_.asInstanceOf[ContinuousFeature].value)),
          standardDeviation(features._2.map(_.asInstanceOf[ContinuousFeature].value))))
      }))
    })
  }

  private def featureCounts(model: Map[T, FeatureMatrix]): Map[T,Map[FeatureColumn,Map[Feature,Int]]] = {
    LaplaceInitialMap(model).map(outcomeEntry => {
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
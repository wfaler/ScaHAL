package org.scahal.classifier

import org.specs2.Specification
import org.scahal.classifier.{Event, ContinuousFeature}
import io.Source
import scalala.tensor.dense.DenseMatrix
import org.specs2.matcher.ThrownExpectations

/**
 * Created with IntelliJ IDEA.
 * User: wfaler
 * Date: 17/04/2012
 * Time: 20:41
 * To change this template use File | Settings | File Templates.
 */

class LogisticRegressionSpec extends Specification with ThrownExpectations { def is =

  "The Logistic Regression spec" ^
  p ^
    "Calculate the correct Gradient Ascent" !gradientAsc ^
    "Classify with increasing accuracy using Gradient Ascent with more iterations" !increasingAccuracy ^
    "Deal with missing features" !missingFeatures ^
    end

  def gradientAsc = {
    val weights = GradientAscent(features)
    weights(0) must be_==(4.124143489627892)
    weights(1) must be_==(0.4800732928842445)
    weights(2) must be_==(-0.6168481970344016)
  }

  def increasingAccuracy = {
    success(GradientAscent(features, maxCycles = 150)) must be_==(84)
    success(GradientAscent(features)) must be_==(91)
    success(GradientAscent(features, maxCycles = 10000)) must be_==(97)
  }

  def missingFeatures = {
    val evs = List(Event(1.0, ContinuousFeature("1", 5) :: ContinuousFeature("2", 5) :: Nil), Event(0.0, ContinuousFeature("1", 8) :: Nil))
    GradientAscent(evs, maxCycles = 50) must have size(2)
  }

  def success(weights: List[Double]) = features.foldLeft(0d)((sum, event) => {
        if(event.outcome == LogisticRegressionClassifier(event.features.map(_.asInstanceOf[ContinuousFeature]), weights)(0).value) sum + 1
        else sum
      })

  lazy val features = Source.fromInputStream(this.getClass.getResourceAsStream("/gradAscent.txt"), "UTF-8").getLines().map(line => {
              val splits = line.split("\t")
              Event(splits(2).trim.toDouble, List(ContinuousFeature("0", 1d), ContinuousFeature("1",
                splits(0).trim.toDouble), ContinuousFeature("2", splits(1).trim.toDouble)))
            }).toList
}

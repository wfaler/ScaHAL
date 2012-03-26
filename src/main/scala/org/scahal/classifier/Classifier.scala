package org.scahal.classifier

/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 26/03/2012
 * Time: 21:22
 * To change this template use File | Settings | File Templates.
 */

trait Classifier {

  /**
   * Classifies a set of features
   * @param features
   * @return outcomes, in order of likelihood.
   */
  def classify(features: Seq[Feature]): List[Outcome]

}

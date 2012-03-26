package org.scahal.classifier

/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 20:05
 * To change this template use File | Settings | File Templates.
 */

case class FeatureMatrix(columns: Set[FeatureColumn], rows: Seq[Seq[Feature]]) {

  def apply(features: Seq[Feature]): FeatureMatrix = {
    FeatureMatrix(features.foldLeft(columns)((cols, feature) => {
      if(cols.contains(FeatureColumn(feature.name, feature.getClass))) cols
      else if(cols.exists(f => f.name == feature.name))
        throw new IllegalStateException("Trying to add feature of class " + feature.getClass.getName + ", but feature precedent has been set to class of " + cols.find(f => f.name == feature.name).get.getClass.getName)
      else cols + FeatureColumn(feature.name, feature.getClass)
    }), rows ++ List(features))
  }

}

case class FeatureColumn(name: String, cls: Class[ _ <: Feature])

object FeatureMatrix{
  def apply(features: Seq[Feature]): FeatureMatrix = FeatureMatrix(features.map(f => FeatureColumn(f.name, f.getClass)).toSet, List(features))
}

object ModelBuilder{
  def apply(events: Seq[Event]): Map[String, FeatureMatrix] = apply(Map[String, FeatureMatrix](), events)

  def apply(model: Map[String, FeatureMatrix], events: Seq[Event]): Map[String, FeatureMatrix] = events.foldLeft(model){ModelBuilder(_,_)}

  def apply(model: Map[String, FeatureMatrix], event: Event): Map[String, FeatureMatrix] = model.get(event.outcome) match{
        case None => model ++ Map(event.outcome -> FeatureMatrix(event.features))
        case Some(matrix) => (model - event.outcome) ++ Map(event.outcome -> matrix(event.features))
      }
}



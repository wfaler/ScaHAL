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
      if(cols.contains(feature.featureColumn)) cols
      else if(cols.exists(f => f.name == feature.name))
        throw new IllegalStateException("Trying to add feature of class " + feature.getClass.getName + ", but feature precedent has been set to class of " + cols.find(f => f.name == feature.name).get.getClass.getName)
      else cols + feature.featureColumn
    }), rows ++ List(features))
  }
         
  def categoricalFeatures(): Map[FeatureColumn, Set[Feature]] = features(classOf[CategoricalFeature[_]]).map(kv => (kv._1, kv._2.toSet))

  private def features(clazz: Class[_ <: Feature]): Map[FeatureColumn, List[Feature]] = {
    columns.filter(_.cls == clazz).foldLeft(Map[FeatureColumn, List[Feature]]())((map, column) => {
      map + (column -> rows.flatMap(seq => {
              seq.find(_.featureColumn == column).map(f => f)
            }).toList)
    })
  }

  def continuousFeatures(): Map[FeatureColumn, List[Feature]] = features(classOf[ContinuousFeature])
}

object FeatureMatrix{
  def apply(features: Seq[Feature]): FeatureMatrix = FeatureMatrix(features.map(f => FeatureColumn(f.name, f.getClass)).toSet, List(features))
}

object AllFeatures{
  def apply(model: Map[String, FeatureMatrix]): Map[FeatureColumn, Set[Feature]] = {
    model.foldLeft(Map[FeatureColumn, Set[Feature]]())((map, tuple) => {
      tuple._2.categoricalFeatures().foldLeft(map)((featureMap, entries) => {
        featureMap.get(entries._1).map(set => featureMap.-(entries._1).+(entries._1 -> (set ++ entries._2))).getOrElse(featureMap + (entries._1 -> entries._2))
      })
    })
  }
}

object LaplaceInitialMap{
  def apply(model: Map[String, FeatureMatrix]): Map[String, Map[FeatureColumn, Map[Feature, Int]]] = {
    val allFeatures = AllFeatures(model)
    model.foldLeft(Map[String, Map[FeatureColumn, Map[Feature, Int]]]())((map, model) => {
       map + (model._1 -> allFeatures.foldLeft(Map[FeatureColumn, Map[Feature, Int]]())((in, feature) => {
         in + (feature._1 -> feature._2.foldLeft(Map[Feature, Int]())((featureMap, feature) => featureMap + (feature -> 1)))
       }))
    })
  }
}


object ModelBuilder{
  def apply(events: Seq[Event]): Map[String, FeatureMatrix] = apply(Map[String, FeatureMatrix](), events)

  def apply(model: Map[String, FeatureMatrix], events: Seq[Event]): Map[String, FeatureMatrix] = events.foldLeft(model){ModelBuilder(_,_)}

  def apply(model: Map[String, FeatureMatrix], event: Event): Map[String, FeatureMatrix] =
    model.get(event.outcome).map(matrix => {
      (model - event.outcome) ++ Map(event.outcome -> matrix(event.features))
    }).getOrElse(model ++ Map(event.outcome -> FeatureMatrix(event.features)))

}

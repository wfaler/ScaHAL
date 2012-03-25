package org.scahal.classifier

/**
 * Created by IntelliJ IDEA.
 * User: wfaler
 * Date: 24/03/2012
 * Time: 20:05
 * To change this template use File | Settings | File Templates.
 */

case class FeatureMatrix(columns: Set[FeatureColumn], rows: Seq[Feature], unfilledColumns: Set[String] = Set[String]()) {

  def apply(features: Seq[Feature]): FeatureMatrix = {
    FeatureMatrix(features.foldLeft(columns)((cols, feature) => {
      if(cols.contains(FeatureColumn(feature.name, feature.getClass))) cols
      else if(cols.exists(f => f.name == feature.name))
        throw new IllegalStateException("Trying to add feature of class " + feature.getClass.getName + ", but feature precedent has been set to class of " + cols.find(f => f.name == feature.name).get.getClass.getName)
      else cols + FeatureColumn(feature.name, feature.getClass)
    }), rows ++ features, missingColumns(features))
  }

  private def missingColumns(features: Seq[Feature]): Set[String] = {
    features.foldLeft(columns.foldLeft(unfilledColumns)((empty, column) => {
      if(features.exists(_.name == column)) unfilledColumns
      else unfilledColumns + column.name
    }))((cols, feature) => {
      if(cols.exists(_==feature.name)) cols
      else cols + feature.name
    })
  }

}

case class FeatureColumn(name: String, cls: Class[ _ <: Feature])

object FeatureMatrix{
  def apply(features: Seq[Feature]): FeatureMatrix = FeatureMatrix(features.map(f => FeatureColumn(f.name, f.getClass)).toSet, features)
}

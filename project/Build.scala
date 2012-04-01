import sbt._
import Keys._

// TODO: Build website project
object MachineLearning extends Build {
  val description = SettingKey[String]("description")
  //import AssemblyKeys._

  resolvers ++= repos

  val parentSettings = Defaults.defaultSettings ++ Seq(
    parallelExecution := false,
    organization := "com.recursivity",
    version := "1.0.0-SNAPSHOT",
    crossScalaVersions := Seq("2.9.1"),
    scalaVersion <<= (crossScalaVersions) {
      versions => versions.head
    },
    packageOptions <<= (packageOptions, name, version, organization) map {
      (opts, title, version, vendor) =>
        opts :+ Package.ManifestAttributes(
          "Created-By" -> "Simple Build Tool",
          "Built-By" -> System.getProperty("user.name"),
          "Build-Jdk" -> System.getProperty("java.version"),
          "Specification-Title" -> title,
          "Specification-Version" -> version,
          "Specification-Vendor" -> vendor,
          "Implementation-Title" -> title,
          "Implementation-Version" -> version,
          "Implementation-Vendor-Id" -> vendor,
          "Implementation-Vendor" -> vendor
        )
    }
  )



  val repos = Seq("Sonatype Nexus releases" at "https://oss.sonatype.org/content/repositories/releases",
    "Sonatype Nexus snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.1")

  val sonatypeSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  object Dependencies {
    //val base64 = "net.iharder" % "base64" % "2.3.8"
    val specs2 = "org.specs2" %% "specs2" % "1.8" % "test"
    val scalaLang = "org.scala-lang" % "scala-library" % "2.9.1"
    val recursivityMaths = "com.recursivity.math" %% "recursivity-maths" % "1.0.0-SNAPSHOT"

  //  val classifier = "com.recursivity" %% "classifier" % "1.0.0-SNAPSHOT"
  }

  import Dependencies._

  lazy val machineLearning = Project("ml", file("."),
    settings = parentSettings ++ seq(sbtassembly.Plugin.assemblySettings: _*))
    .settings(libraryDependencies := Seq(specs2, scalaLang, recursivityMaths),
    publishArtifact in Compile := false,
    description := "Parent project",
	resolvers ++= repos)
}
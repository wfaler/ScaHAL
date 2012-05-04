import sbt._
import Keys._

// TODO: Build website project
object MachineLearning extends Build {
  val description = SettingKey[String]("description")
  //import AssemblyKeys._

  resolvers ++= repos

  val parentSettings = Defaults.defaultSettings ++ Seq(
    parallelExecution := false,
    organization := "org.scahal",
    version := "1.0.0-SNAPSHOT",
    crossScalaVersions := Seq("2.9.2"),
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
    "Sonatype Nexus snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.1")

  val sonatypeSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  object Dependencies {
    //val base64 = "net.iharder" % "base64" % "2.3.8"
    val specs2 = "org.specs2" %% "specs2" % "1.9" % "test"
    val scalala = "org.scalala" % "scalala_2.9.2" % "1.0.0.RC3-SNAPSHOT"

  //  val classifier = "com.recursivity" %% "classifier" % "1.0.0-SNAPSHOT"
  }

  import Dependencies._

  lazy val machineLearning = Project("scahal", file("."),
    settings = parentSettings)
    .settings(libraryDependencies := Seq(scalala,specs2),
//    publishArtifact in Compile := false,
    description := "Parent project",
	resolvers ++= repos)
}

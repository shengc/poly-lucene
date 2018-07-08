name := "poly-lucene"

scalaVersion := "2.11.12"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
, "org.apache.lucene" % "lucene-core" % "7.2.1"
, "org.apache.lucene" % "lucene-analyzers-common" % "7.2.1"
, "org.apache.lucene" % "lucene-facet" % "7.2.1"
, "org.scalatest" %% "scalatest" % "3.0.5" % Test
, "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
)

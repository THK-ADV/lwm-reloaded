import sbt.Keys._
import sbt._

name := """lwm-reloaded"""

version := "1.0-SNAPSHOT"

lazy val sesameVersion = "2.7.15"
lazy val bananaVersion = "0.8.1"

lazy val commonSettings = Seq(
  name := "lwm-semantics",
  version := "1.0",
  organization := "lwm",
  version := "0.1.0",
  scalaVersion := "2.11.6"
)

lazy val root = (project in file(".")).
  settings(Defaults.coreDefaultSettings: _*).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= semanticDependencies,
    libraryDependencies ++= testDependencies
  ).
  enablePlugins(PlayScala)

lazy val semanticDependencies = Seq(
  "org.w3" %% "banana-rdf" % bananaVersion,
  "org.w3" %% "banana-sesame" % bananaVersion,
  "org.openrdf.sesame" % "sesame-runtime" % sesameVersion
)

lazy val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalactic" %% "scalactic" % "2.2.4"
)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  cache,
  ws
)

libraryDependencies += "com.unboundid" % "unboundid-ldapsdk" % "2.3.6" withSources() withJavadoc()

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc()

libraryDependencies += "org.scalatestplus" % "play_2.11" % "1.2.0" withSources() withJavadoc()

libraryDependencies += "org.mockito" % "mockito-core" % "2.0.7-beta" withSources() withJavadoc()
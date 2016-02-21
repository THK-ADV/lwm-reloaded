import sbt.Keys._
import sbt._

name := """lwm-reloaded"""

version := "1.0-SNAPSHOT"

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

lazy val sesameVersion = "2.7.15"
lazy val bananaVersion = "0.8.1"
lazy val scalazVersion = "7.1.2"
lazy val scalatestVersion = "2.2.4"
lazy val scalacheckVersion = "1.12.5"

lazy val commonSettings = Seq(
  name := "lwm-semantics",
  version := "1.0",
  organization := "lwm",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(Defaults.coreDefaultSettings: _*).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= semanticDependencies,
    libraryDependencies ++= lwmDependencies,
    libraryDependencies ++= scalazDependencies,
    libraryDependencies ++= testDependencies
  ).enablePlugins(PlayScala)

lazy val semanticDependencies = Seq(
  "org.w3" %% "banana-rdf" % bananaVersion,
  "org.w3" %% "banana-sesame" % bananaVersion,
  "org.openrdf.sesame" % "sesame-runtime" % sesameVersion
)

lazy val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % "test",
  "org.scalactic" %% "scalactic" % scalatestVersion % "test",
  "org.scalatestplus" %% "play" % "1.4.0-M3" % "test",
  "org.mockito" % "mockito-core" % "2.0.8-beta" % "test",
  "com.typesafe.akka" % "akka-testkit_2.11" % "2.4.0"
)

lazy val scalazDependencies = Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion
)

lazy val lwmDependencies = Seq(
  "com.chuusai" %% "shapeless" % "2.2.5",
  "com.unboundid" % "unboundid-ldapsdk" % "2.3.6"
)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  cache,
  ws,
  specs2,
  json,
  filters
)

routesGenerator := InjectedRoutesGenerator

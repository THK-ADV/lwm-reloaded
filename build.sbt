import sbt._
import play.PlayImport.PlayKeys._


name := """lwm-reloaded"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  cache,
  ws
)

libraryDependencies += "com.unboundid" % "unboundid-ldapsdk" % "2.3.6" withSources() withJavadoc()

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc()

libraryDependencies += "org.scalatestplus" % "play_2.11" % "1.2.0" withSources() withJavadoc()

libraryDependencies += "org.mockito" % "mockito-core" % "2.0.7-beta" withSources() withJavadoc()

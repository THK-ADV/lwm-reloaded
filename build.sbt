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

libraryDependencies += "org.mockito" % "mockito-core" % "2.0.7-beta" withSource() withJavaDoc()

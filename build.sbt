name := """lwm-reloaded"""

version := "1.0-SNAPSHOT"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

lazy val scalazVersion = "7.1.12"
lazy val scalatestVersion = "3.0.5"
lazy val slickVersion = "3.3.0"

lazy val commonSettings = Seq(
  name := "lwm-reloaded",
  version := "1.0",
  organization := "lwm",
  version := "0.1.0",
  scalaVersion := "2.12.6"
)

lazy val root = (project in file(".")).
  settings(Defaults.coreDefaultSettings: _*).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= lwmDependencies,
    libraryDependencies ++= scalazDependencies,
    libraryDependencies ++= testDependencies,
    libraryDependencies ++= postgresDependencies,
    libraryDependencies ++= keycloakDepencencies
  ).enablePlugins(PlayScala)


lazy val testDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "org.scalactic" %% "scalactic" % scalatestVersion,
  "org.scalatest" %% "scalatest" % scalatestVersion % "test",
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % "test",
  "org.mockito" % "mockito-core" % "2.23.4"
  //  "com.typesafe.akka" % "akka-testkit_2.11" % "2.4.0"
)

lazy val scalazDependencies = Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion
)

lazy val lwmDependencies = Seq(
  "com.typesafe.play" %% "play-json" % "2.6.10",
  "commons-io" % "commons-io" % "2.6"
)

lazy val postgresDependencies = Seq(
  "com.typesafe.slick" %% "slick" % slickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion,
  "org.postgresql" % "postgresql" % "9.4-1206-jdbc42"
)

val keycloakDepencencies = Seq(
  "org.keycloak" % "keycloak-core" % "4.7.0.Final",
  "org.keycloak" % "keycloak-adapter-core" % "4.7.0.Final",
  "org.jboss.logging" % "jboss-logging" % "3.3.0.Final",
  "org.apache.httpcomponents" % "httpclient" % "4.5.1"
)

libraryDependencies ++= Seq(ws, guice)

routesGenerator := InjectedRoutesGenerator

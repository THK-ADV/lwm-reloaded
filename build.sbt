name := """lwm-reloaded"""

version := "1.0-SNAPSHOT"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
//resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
//resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
//resolvers += "theatr.us" at "http://repo.theatr.us"

lazy val scalazVersion = "7.1.12"

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
  //  "org.scalacheck" %% "scalacheck" % scalacheckVersion % "test",
  //  "org.scalatest" %% "scalatest" % scalatestVersion % "test",
  //  "org.scalactic" %% "scalactic" % scalatestVersion % "test",
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
//  "org.mockito" % "mockito-core" % "2.0.8-beta" % "test",
//  "com.typesafe.akka" % "akka-testkit_2.11" % "2.4.0"
)

lazy val scalazDependencies = Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion
)

lazy val lwmDependencies = Seq(
//  "com.chuusai" %% "shapeless" % "2.2.5",
  "com.unboundid" % "unboundid-ldapsdk" % "2.3.6" % Test,
//  "us.theatr" %% "akka-quartz" % "0.3.0",
  "com.typesafe.play" %% "play-json" % "2.6.10"
)

lazy val postgresDependencies = Seq(
  "com.typesafe.slick" %% "slick" % "3.2.0",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.0",
  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41"
)

val keycloakDepencencies = Seq(
  "org.keycloak" % "keycloak-core" % "4.7.0.Final",
  "org.keycloak" % "keycloak-adapter-core" % "4.7.0.Final",
  "org.jboss.logging" % "jboss-logging" % "3.3.0.Final",
  "org.apache.httpcomponents" % "httpclient" % "4.5.1"
)

libraryDependencies ++= Seq(ws, guice)

routesGenerator := InjectedRoutesGenerator

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val scalazVersion = "7.1.17"
lazy val scalatestVersion = "3.0.5"
lazy val slickVersion = "3.4.1"
lazy val mailVersion = "8.0.1"
lazy val postgresVersion = "42.5.1"

lazy val commonSettings = Seq(
  name := "lwm-reloaded",
  version := "1.0",
  organization := "lwm",
  scalaVersion := "2.12.15",
  maintainer := "alexander.dobrynin@th-koeln.de"
)

lazy val root = (project in file(".")).
  settings(Defaults.coreDefaultSettings: _*).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= lwmDependencies,
    libraryDependencies ++= scalazDependencies,
    libraryDependencies ++= testDependencies,
    libraryDependencies ++= postgresDependencies,
    libraryDependencies ++= keycloakDepencencies,
    libraryDependencies ++= mailDependencies,
  ).enablePlugins(PlayScala)


lazy val testDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "org.scalactic" %% "scalactic" % scalatestVersion,
  "org.scalatest" %% "scalatest" % scalatestVersion % "test",
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % "test",
  "org.mockito" % "mockito-core" % "2.23.4",
  "com.typesafe.akka" %% "akka-testkit" % "2.6.14" % Test
)

lazy val scalazDependencies = Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion
)

lazy val lwmDependencies = Seq(
  "com.typesafe.play" %% "play-json" % "2.9.3",
  "commons-io" % "commons-io" % "2.6",
  "org.apache.poi" % "poi" % "4.1.2",
  "joda-time" % "joda-time" % "2.12.2"
)

lazy val postgresDependencies = Seq(
  "com.typesafe.slick" %% "slick" % slickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion,
  "org.postgresql" % "postgresql" % postgresVersion
)

val keycloakDepencencies = Seq(
  "org.keycloak" % "keycloak-core" % "4.7.0.Final",
  "org.keycloak" % "keycloak-adapter-core" % "4.7.0.Final",
  "org.jboss.logging" % "jboss-logging" % "3.3.0.Final",
  "org.apache.httpcomponents" % "httpclient" % "4.5.1"
)

val mailDependencies = Seq(
  "com.typesafe.play" %% "play-mailer" % mailVersion,
  "com.typesafe.play" %% "play-mailer-guice" % mailVersion
)

libraryDependencies ++= Seq(ws, guice)

routesGenerator := InjectedRoutesGenerator

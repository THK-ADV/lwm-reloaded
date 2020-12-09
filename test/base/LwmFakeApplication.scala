package base

import akka.stream.Materializer
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}

trait LwmFakeApplication {
  self: GuiceOneAppPerSuite =>

  val fakeDbConfig = Configuration(
    "database.properties.url" -> "jdbc:postgresql://localhost:5432/alex",
    "database.properties.user" -> "alex",
    "database.properties.databaseName" -> "alex",
    "database.properties.password" -> ""
  )

  implicit lazy val materializer: Materializer = app.materializer

  // import play.api.inject.bind
  protected def bindings: Seq[GuiceableModule]

  override def fakeApplication() = new GuiceApplicationBuilder()
    .configure(fakeDbConfig)
    .overrides(bindings: _*)
    .build
}

package base

import akka.stream.Materializer
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}

trait LwmFakeApplication {
  self: GuiceOneAppPerSuite =>

  val fakeDbConfig = Configuration(
    "database.properties.url" -> "jdbc:postgresql://139.6.56.147:5432/lwm_spec",
    "database.properties.user" -> "lwm",
    "database.properties.databaseName" -> "lwm_spec",
    "database.properties.password" -> "abcde123"
  )

  implicit lazy val materializer: Materializer = app.materializer

  // import play.api.inject.bind
  protected def bindings: Seq[GuiceableModule]

  override def fakeApplication() = new GuiceApplicationBuilder()
    .configure(fakeDbConfig)
    .overrides(bindings: _*)
    .build
}

package base

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.inject.guice.GuiceApplicationBuilder

trait LwmFakeApplication {
  self: GuiceOneAppPerSuite =>

  val fakeDbConfig = Configuration(
    "database.properties.url" -> "jdbc:postgresql://localhost:5432/lwm",
    "database.properties.databaseName" -> "lwm",
    "database.properties.user" -> "postgres",
    "database.properties.password" -> ""
  )

  override def fakeApplication() = new GuiceApplicationBuilder()
    .configure(fakeDbConfig)
    .build()
}

package base

import akka.stream.Materializer
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}

trait LwmFakeApplication {
  self: GuiceOneAppPerSuite =>

  val fakeDbConfig = Configuration(
    "database.properties.url" -> "jdbc:postgresql://localhost:5432/lwm_test",
    "database.properties.user" -> "postgres",
    "database.properties.databaseName" -> "lwm_test",
    "database.properties.password" -> "",
    "keycloak.api.baseUrl" -> "TODO",
    "keycloak.api.realm" -> "TODO",
    "keycloak.api.admin-cli.clientId" -> "TODO",
    "keycloak.api.admin-cli.clintSecret" -> "TODO"
  )

  implicit lazy val materializer: Materializer = app.materializer

  // import play.api.inject.bind
  protected def bindings: Seq[GuiceableModule]

  override def fakeApplication() = new GuiceApplicationBuilder()
    .configure(fakeDbConfig)
    .overrides(bindings: _*)
    .build
}

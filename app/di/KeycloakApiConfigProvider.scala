package di

import keycloakapi.KeycloakApiConfig
import play.api.Configuration

import javax.inject.{Inject, Provider}

class KeycloakApiConfigProvider @Inject() (implicit
    val config: Configuration
) extends Provider[KeycloakApiConfig]
    with ConfigReader {
  override lazy val get =
    KeycloakApiConfig(
      nonEmptyConfig("keycloak.api.baseUrl") getOrElse "",
      nonEmptyConfig("keycloak.api.realm") getOrElse "",
      nonEmptyConfig("keycloak.api.admin-cli.clientId") getOrElse "",
      nonEmptyConfig("keycloak.api.admin-cli.clintSecret") getOrElse ""
    )
}

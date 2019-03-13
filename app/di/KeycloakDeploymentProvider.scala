package di

import javax.inject.{Provider, Singleton}
import org.keycloak.adapters.{KeycloakDeployment, KeycloakDeploymentBuilder}
import play.api.Play

@Singleton
class KeycloakDeploymentProvider extends Provider[KeycloakDeployment] {
  lazy val get = KeycloakDeploymentBuilder.build(
    Play.getClass.getResourceAsStream("/keycloak.json")
  )
}
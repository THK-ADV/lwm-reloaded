import auth.{KeycloakAuthorization, OAuthAuthorization}
import com.google.inject.AbstractModule
import dao._
import org.keycloak.adapters.{KeycloakDeployment, KeycloakDeploymentBuilder}
import play.api.Play
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
/**
  * This class is a Guice module that tells Guice how to bind several
  * different types. This Guice module is created when the Play
  * application starts.
  *
  * Play will automatically use any class called `Module` that is in
  * the root package. You can create modules in other locations by
  * adding `play.modules.enabled` settings to the `application.conf`
  * configuration file.
  */
class Module extends AbstractModule {

  override def configure() = {
    bind(classOf[SemesterDao]).to(classOf[SemesterDaoImpl])
    bind(classOf[AuthorityDao]).to(classOf[AuthorityDaoImpl])
    bind(classOf[RoleDao]).to(classOf[RoleDaoImpl])

    bind(classOf[OAuthAuthorization]).to(classOf[KeycloakAuthorization])
    bind(classOf[KeycloakDeployment]).toInstance(KeycloakDeploymentBuilder.build(
      Play.getClass.getResourceAsStream("/keycloak.json")
    ))

    bind(classOf[PostgresProfile.backend.Database]).toInstance(Database.forConfig("database")) // this seems to be a singleton... which is what we need
  }

}
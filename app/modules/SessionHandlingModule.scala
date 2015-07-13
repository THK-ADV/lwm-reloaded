package modules

import controllers.SessionController
import services.{ActorBasedSessionService, SessionHandlingService}
import utils.{Authenticator, LDAPAuthentication}


trait AuthenticatorModule {
  def authenticator: Authenticator
}


trait LDAPAuthenticatorModule extends AuthenticatorModule {
  self: ConfigurationModule =>
  val bindHost: String = lwmConfig.getString("lwm.bindHost").getOrElse("no bind host set")
  val bindPort: Int = lwmConfig.getInt("lwm.bindPort").getOrElse(-1)
  val dn: String = lwmConfig.getString("lwm.bindDN").getOrElse("no dn set")


  override def authenticator: Authenticator = LDAPAuthentication(bindHost, bindPort, dn)
}

trait SessionControllerModule {
  self: SessionRepositoryModule =>
  def sessionController: SessionController = new SessionController(sessionService)
}

trait SessionRepositoryModule {
  def sessionService: SessionHandlingService
}

trait DefaultSessionRepositoryModuleImpl extends SessionRepositoryModule {
  self: AkkaActorSystemModule with AuthenticatorModule =>

  override def sessionService: SessionHandlingService = new ActorBasedSessionService(system, authenticator)
}

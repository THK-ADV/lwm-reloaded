package modules

import controllers.SessionController
import modules.store.{ResolversModule, SemanticRepositoryModule}
import services.{ActorBasedSessionService, LDAPService, LDAPServiceImpl, SessionHandlingService}

trait LDAPModule {
  def ldap: LDAPService
}

trait LDAPModuleImpl extends LDAPModule {
  self: ConfigurationModule =>
  val bindHost: String = lwmConfig.getString("lwm.bindHost").getOrElse("no bind host set")
  val bindPort: Int = lwmConfig.getInt("lwm.bindPort").getOrElse(-1)
  val dn: String = lwmConfig.getString("lwm.bindDN").getOrElse("no dn set")


  override def ldap: LDAPService = LDAPServiceImpl(bindHost, bindPort, dn)
}

trait SessionControllerModule {
  self: SessionRepositoryModule =>
  def sessionController: SessionController
}

trait DefaultSessionControllerModuleImpl extends SessionControllerModule{ self: SessionRepositoryModule =>
  override def sessionController: SessionController = new SessionController(sessionService)
}

trait SessionRepositoryModule {
  def sessionService: SessionHandlingService
}

trait DefaultSessionRepositoryModuleImpl extends SessionRepositoryModule {
  self: AkkaActorSystemModule with LDAPModule with SemanticRepositoryModule with ResolversModule =>

  override def sessionService: SessionHandlingService = new ActorBasedSessionService(system, ldap, resolver)
}

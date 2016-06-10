package modules

import controllers.SessionController
import modules.store.{ResolversModule, SemanticRepositoryModule}
import services.{ActorBasedSessionService, LDAPService, LDAPServiceImpl, SessionHandlingService}

trait LDAPModule {

  def ldapService: LDAPService
}

trait LDAPModuleImpl extends LDAPModule {
  self: ConfigurationModule =>
  val bindHost: String = lwmConfig.getString("lwm.ldap.bindHost").getOrElse("no bind host set")
  val bindPort: Int = lwmConfig.getInt("lwm.ldap.bindPort").getOrElse(-1)
  val dn: String = lwmConfig.getString("lwm.ldap.bindDN").getOrElse("no dn set")

  override def ldapService = LDAPServiceImpl(bindHost, bindPort, dn)
}

trait SessionControllerModule {
  self: SessionRepositoryModule =>

  def sessionController: SessionController
}

trait DefaultSessionControllerModuleImpl extends SessionControllerModule {
  self: SessionRepositoryModule =>

  override def sessionController: SessionController = new SessionController(sessionService)
}

trait SessionRepositoryModule {
  def sessionService: SessionHandlingService
}

trait DefaultSessionRepositoryModuleImpl extends SessionRepositoryModule {
  self: AkkaActorSystemModule with LDAPModule with SemanticRepositoryModule with ResolversModule =>

  override val sessionService: SessionHandlingService = new ActorBasedSessionService(system, ldapService, resolvers)
}

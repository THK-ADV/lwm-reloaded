package modules

import controllers.SessionController
import modules.store.{ResolversModule, SemanticRepositoryModule}
import services.{ActorBasedSessionService, LdapService, LdapServiceImpl, SessionHandlingService}

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
  self: AkkaActorSystemModule with LdapModule with SemanticRepositoryModule with ResolversModule =>

  override val sessionService: SessionHandlingService = new ActorBasedSessionService(system, ldapService, resolvers)
}

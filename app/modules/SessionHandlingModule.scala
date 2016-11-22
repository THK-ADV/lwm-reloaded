package modules

import controllers.SessionController
import services.{ActorBasedSessionService, SessionHandlingService}

trait SessionControllerModule {
  self: SessionRepositoryModule =>

  def sessionController: SessionController
}

trait DefaultSessionControllerModuleImpl extends SessionControllerModule {
  self: SessionRepositoryModule =>

  override lazy val sessionController: SessionController = new SessionController(sessionService)
}

trait SessionRepositoryModule {
  def sessionService: SessionHandlingService
}

trait DefaultSessionRepositoryModuleImpl extends SessionRepositoryModule {
  self: AkkaActorSystemModule with LdapModule with SemanticRepositoryModule with ResolversModule =>

  override lazy val sessionService: SessionHandlingService = new ActorBasedSessionService(system, ldapService, resolvers)
}

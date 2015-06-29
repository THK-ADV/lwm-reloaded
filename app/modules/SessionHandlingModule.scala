package modules

import controllers.SessionController
import services.{ActorBasedSessionService, SessionHandlingService}


trait SessionControllerModule {
  self: SessionRepositoryModule =>
  def sessionController: SessionController = new SessionController(sessionService)
}

trait SessionRepositoryModule {
  def sessionService: SessionHandlingService
}

trait DefaultSessionRepositoryModuleImpl extends SessionRepositoryModule {
  self: AkkaActorSystemModule =>

  override def sessionService: SessionHandlingService = new ActorBasedSessionService(system)
}
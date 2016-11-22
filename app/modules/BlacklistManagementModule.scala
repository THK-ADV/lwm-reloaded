package modules

import controllers.BlacklistCRUDController
import services.{BlacklistService, BlacklistServiceLike}
import utils.LwmApplication

trait BlacklistServiceManagementModule {
  self: LwmApplication =>

  def blacklistService: BlacklistServiceLike
}

trait DefaultBlacklistServiceManagementModule extends BlacklistServiceManagementModule {
  self: LwmApplication =>

  lazy val blacklistService: BlacklistServiceLike = new BlacklistService
}

trait BlacklistManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule with BlacklistServiceManagementModule =>

  def blacklistManagementController: BlacklistCRUDController
}

trait DefaultBlacklistManagementModuleImpl extends BlacklistManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with BlacklistServiceManagementModule =>

  lazy val blacklistManagementController: BlacklistCRUDController = new BlacklistCRUDController(repository, sessionService, namespace, roleService, blacklistService)
}



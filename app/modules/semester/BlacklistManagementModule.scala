package modules.semester

import controllers.crud.semester.BlacklistCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services.{BlacklistService, BlacklistServiceLike}
import utils.LwmApplication

trait BlacklistServiceManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>

  def blacklistService: BlacklistServiceLike
}

trait DefaultBlacklistServiceManagementModule extends BlacklistServiceManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>

  lazy val blacklistService: BlacklistServiceLike = new BlacklistService(repository)
}

trait BlacklistManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def blacklistManagementController: BlacklistCRUDController
}

trait DefaultBlacklistManagementModuleImpl extends BlacklistManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val blacklistManagementController: BlacklistCRUDController = new BlacklistCRUDController(repository, namespace, roleService)
}



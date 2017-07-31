package modules

import controllers.{BlacklistCRUDController, BlacklistControllerPostgres}
import dao.{BlacklistService2, BlacklistServiceImpl}
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

// POSTGRES

trait BlacklistService2ManagementModule {
  self: DatabaseModule =>

  def blacklistService2: BlacklistService2
}

trait DefaultBlacklistService2ManagementModule extends BlacklistService2ManagementModule {
  self: DatabaseModule =>

  override lazy val blacklistService2 = new BlacklistServiceImpl(db)
}

trait Blacklist2ManagementModule {
  self: SecurityManagementModule with SessionRepositoryModule with BlacklistService2ManagementModule with BlacklistServiceManagementModule =>

  def blacklistControllerPostgres: BlacklistControllerPostgres
}

trait DefaultBlacklist2ManagementModule extends Blacklist2ManagementModule {
  self: SecurityManagementModule with SessionRepositoryModule with BlacklistService2ManagementModule with BlacklistServiceManagementModule =>

  override lazy val blacklistControllerPostgres = new BlacklistControllerPostgres(roleService, sessionService, blacklistService2, blacklistService)
}
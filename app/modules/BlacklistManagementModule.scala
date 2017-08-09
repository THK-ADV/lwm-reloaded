package modules

import controllers.{BlacklistCRUDController, BlacklistControllerPostgres}
import dao.{BlacklistDao, BlacklistDaoImpl}
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

trait BlacklistDaoManagementModule { self: DatabaseModule =>
  def blacklistDao: BlacklistDao
}

trait DefaultBlacklistDaoManagementModule extends BlacklistDaoManagementModule { self: DatabaseModule =>
  override lazy val blacklistDao = new BlacklistDaoImpl(db)
}

trait Blacklist2ManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with BlacklistDaoManagementModule with BlacklistServiceManagementModule =>

  def blacklistControllerPostgres: BlacklistControllerPostgres
}

trait DefaultBlacklist2ManagementModule extends Blacklist2ManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with BlacklistDaoManagementModule with BlacklistServiceManagementModule =>

  override lazy val blacklistControllerPostgres = new BlacklistControllerPostgres(authorityDao, sessionService, blacklistDao, blacklistService)
}
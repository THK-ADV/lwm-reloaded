package modules

import controllers.{BlacklistCRUDController, BlacklistControllerPostgres}
import dao.{BlacklistDao, BlacklistDaoImpl}

trait BlacklistManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def blacklistManagementController: BlacklistCRUDController
}

trait DefaultBlacklistManagementModuleImpl extends BlacklistManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val blacklistManagementController: BlacklistCRUDController = new BlacklistCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait BlacklistDaoManagementModule { self: DatabaseModule =>
  def blacklistDao: BlacklistDao
}

trait DefaultBlacklistDaoManagementModule extends BlacklistDaoManagementModule { self: DatabaseModule =>
  override lazy val blacklistDao = new BlacklistDaoImpl(db)
}

trait Blacklist2ManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with BlacklistDaoManagementModule =>

  def blacklistControllerPostgres: BlacklistControllerPostgres
}

trait DefaultBlacklist2ManagementModule extends Blacklist2ManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with BlacklistDaoManagementModule =>

  override lazy val blacklistControllerPostgres = new BlacklistControllerPostgres(authorityDao, sessionService, blacklistDao)
}
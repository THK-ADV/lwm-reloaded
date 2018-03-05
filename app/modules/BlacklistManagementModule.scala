package modules

import controllers.BlacklistControllerPostgres
import dao.{BlacklistDao, BlacklistDaoImpl}
import services.blacklist.{BlacklistService, BlacklistServiceImpl}


trait BlacklistServiceModule {
  def blacklistService: BlacklistService
}

trait DefaultBlacklistServiceModule {
  lazy val blacklistService = new BlacklistServiceImpl()
}

trait BlacklistDaoManagementModule {
  self: DatabaseModule =>

  def blacklistDao: BlacklistDao
}

trait DefaultBlacklistDaoManagementModule extends BlacklistDaoManagementModule {
  self: DatabaseModule =>

  override lazy val blacklistDao = new BlacklistDaoImpl(db)
}

trait Blacklist2ManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with BlacklistDaoManagementModule with BlacklistServiceModule =>

  def blacklistControllerPostgres: BlacklistControllerPostgres
}

trait DefaultBlacklist2ManagementModule extends Blacklist2ManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with BlacklistDaoManagementModule with BlacklistServiceModule =>

  override lazy val blacklistControllerPostgres = new BlacklistControllerPostgres(authorityDao, sessionService, blacklistDao, blacklistService)
}
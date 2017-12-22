package modules

import controllers.BlacklistControllerPostgres
import dao.{BlacklistDao, BlacklistDaoImpl}


trait BlacklistDaoManagementModule {
  self: DatabaseModule =>

  def blacklistDao: BlacklistDao
}

trait DefaultBlacklistDaoManagementModule extends BlacklistDaoManagementModule {
  self: DatabaseModule =>

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
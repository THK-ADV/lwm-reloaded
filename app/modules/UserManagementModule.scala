package modules

import controllers.UserControllerPostgres
import dao.{UserDao, UserDaoImpl}

trait UserDaoModule {
  self: DatabaseModule with AuthorityDaoModule with DegreeDaoModule with LabworkApplicationDaoModule =>

  def userDao: UserDao
}

trait DefaultUserDaoModule extends UserDaoModule {
  self: DatabaseModule with AuthorityDaoModule with DegreeDaoModule with LabworkApplicationDaoModule =>

  override lazy val userDao = new UserDaoImpl(db, authorityDao, degreeDao, labworkApplicationDao)
}

trait UserManagementModulePostgres {

  def userControllerPostgres: UserControllerPostgres
}

trait DefaultUserManagementModulePostgres extends UserManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with LdapModule with UserDaoModule =>

  override lazy val userControllerPostgres: UserControllerPostgres = new UserControllerPostgres(authorityDao, sessionService, ldapService, userDao)
}
package modules

import controllers.{UserController, UserControllerPostgres}
import dao.{UserDao, UserDaoImpl}

trait UserManagementModule {

  def userController: UserController
}

trait DefaultUserManagementModule extends UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace with SessionRepositoryModule with ResolversModule with LdapModule =>

  override lazy val userController: UserController = new UserController(roleService, sessionService, repository, namespace, resolvers, ldapService)
}

// POSTGRES

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
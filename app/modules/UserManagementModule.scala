package modules

import controllers.{UserController, UserControllerPostgres}
import dao.{UserService, UserServiceImpl}

trait UserManagementModule {

  def userController: UserController
}

trait DefaultUserManagementModule extends UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace with SessionRepositoryModule with ResolversModule with LdapModule =>

  override lazy val userController: UserController = new UserController(roleService, sessionService, repository, namespace, resolvers, ldapService)
}

// POSTGRES

trait UserServiceModule {
  self: DatabaseModule with AuthorityServiceModule with DegreeServiceModule with LabworkApplication2ServiceModule =>

  def userService: UserService
}

trait DefaultUserServiceModule extends UserServiceModule {
  self: DatabaseModule with AuthorityServiceModule with DegreeServiceModule with LabworkApplication2ServiceModule =>

  override lazy val userService = new UserServiceImpl(db, authorityService, degreeService, labworkApplicationService2)
}

trait UserManagementModulePostgres {

  def userControllerPostgres: UserControllerPostgres
}

trait DefaultUserManagementModulePostgres extends UserManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with LdapModule with UserServiceModule =>

  override lazy val userControllerPostgres: UserControllerPostgres = new UserControllerPostgres(roleService, sessionService, ldapService, userService)
}
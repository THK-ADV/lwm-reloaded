package modules

import controllers.{UserController, UserControllerPostgres}
import services.UserService

trait UserManagementModule {

  def userController: UserController
}

trait UserManagementModulePostgres {

  def userControllerPostgres: UserControllerPostgres
}

trait DefaultUserManagementModule extends UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace with SessionRepositoryModule with ResolversModule with LdapModule =>

  override lazy val userController: UserController = new UserController(roleService, sessionService, repository, namespace, resolvers, ldapService)
}

trait DefaultUserManagementModulePostgres extends UserManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with ResolversModule with LdapModule =>

  override lazy val userControllerPostgres: UserControllerPostgres = new UserControllerPostgres(roleService, sessionService, resolvers, ldapService, UserService)
}

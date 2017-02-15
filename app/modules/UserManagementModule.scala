package modules

import controllers.{UserController, UserControllerP}
import services.UserService

trait UserManagementModule {

  def userController: UserControllerP
  //def userController: UserController
}

trait DefaultUserManagementModule extends UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace with SessionRepositoryModule with ResolversModule with LdapModule =>

  //override lazy val userController: UserController = new UserController(roleService, sessionService, repository, namespace, resolvers, ldapService)
  override lazy val userController: UserControllerP = new UserControllerP(roleService, sessionService, resolvers, ldapService, UserService)
}

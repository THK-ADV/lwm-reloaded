package modules

import controllers.UserController

trait UserManagementModule {

  def userController: UserController
}

trait DefaultUserManagementModule extends UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace with SessionRepositoryModule with ResolversModule with LdapModule =>

  override lazy val userController: UserController = new UserController(roleService, sessionService, repository, namespace, resolvers, ldapService)
}

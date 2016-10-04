package modules

import controllers.UserController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, ResolversModule, SemanticRepositoryModule}

trait UserManagementModule {

  def userController: UserController
}

trait DefaultUserManagementModule extends UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace with SessionRepositoryModule with ResolversModule with LdapModule =>

  override def userController: UserController = new UserController(roleService, sessionService, repository, namespace, resolvers, ldapService)
}

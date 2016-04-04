package modules

import controllers.UserController
import modules.security.SecurityManagementModule
import store.{BaseNamespace, SemanticRepositoryModule}

trait UserManagementModule {

  def userController: UserController
}

trait DefaultUserManagementModule extends UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace with SessionRepositoryModule with LDAPModule =>

  override def userController: UserController = new UserController(roleService, sessionService, repository, namespace, ldap)
}

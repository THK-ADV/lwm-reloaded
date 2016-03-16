package modules

import controllers.UserController
import modules.security.SecurityManagementModule
import store.{BaseNamespace, SemanticRepositoryModule}


trait UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace =>
  def userController: UserController
}

trait DefaultUserManagementModule extends UserManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with BaseNamespace =>
  override def userController: UserController = new UserController(roleService, repository, namespace)
}

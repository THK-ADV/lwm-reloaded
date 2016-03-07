package modules.security

import controllers.security.RoleController
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait RoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def roleManagementController: RoleController
}

trait DefaultRoleManagementModule extends RoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  override def roleManagementController: RoleController = new RoleController(repository, namespace, roleService)
}

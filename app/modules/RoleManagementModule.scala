package modules

import controllers.crud.RoleCRUDController

trait RoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def roleManagementController: RoleCRUDController
}

trait DefaultRoleManagementModule extends RoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  override def roleManagementController: RoleCRUDController = new RoleCRUDController(repository, namespace, roleService)
}

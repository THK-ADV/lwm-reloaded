package modules

import controllers.crud.RefRoleCRUDController

trait RefRoleManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>

  def refRoleManagementController: RefRoleCRUDController
}

trait DefaultRefRoleManagementModuleImpl extends RefRoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>

  lazy val refRoleManagementController: RefRoleCRUDController = new RefRoleCRUDController(repository, namespace, roleService)
}
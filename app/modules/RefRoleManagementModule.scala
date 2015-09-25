package modules

import controllers.crud.RefRoleCRUDController

trait RefRoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def refRoleManagementController: RefRoleCRUDController
}

trait DefaultRefRoleManagementModuleImpl extends RefRoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val refRoleManagementController: RefRoleCRUDController = new RefRoleCRUDController(repository, namespace, roleService)
}
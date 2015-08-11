package modules

import controllers.crud.GroupCRUDController


trait GroupManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def groupManagementController: GroupCRUDController
}

trait DefaultGroupManagementModuleImpl extends GroupManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val groupManagementController: GroupCRUDController = new GroupCRUDController(repository, namespace, roleService)
}

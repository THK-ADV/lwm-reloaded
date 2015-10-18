package modules

import controllers.crud.GroupCRUDController


trait GroupManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def groupManagementController: GroupCRUDController
}

trait DefaultGroupManagementModuleImpl extends GroupManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val groupManagementController: GroupCRUDController = new GroupCRUDController(repository, namespace, roleService)
}

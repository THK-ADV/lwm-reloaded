package modules

import controllers.crud.GroupScheduleAssociationCRUDController


trait GroupScheduleAssociationManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def groupScheduleAssociationManagementController: GroupScheduleAssociationCRUDController
}

trait DefaultGroupScheduleAssociationManagementModuleImpl extends GroupScheduleAssociationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val groupScheduleAssociationManagementController: GroupScheduleAssociationCRUDController = new GroupScheduleAssociationCRUDController(repository, namespace, roleService)
}

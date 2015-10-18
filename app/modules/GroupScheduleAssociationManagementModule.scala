package modules

import controllers.crud.GroupScheduleAssociationCRUDController


trait GroupScheduleAssociationManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def groupScheduleAssociationManagementController: GroupScheduleAssociationCRUDController
}

trait DefaultGroupScheduleAssociationManagementModuleImpl extends GroupScheduleAssociationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val groupScheduleAssociationManagementController: GroupScheduleAssociationCRUDController = new GroupScheduleAssociationCRUDController(repository, namespace, roleService)
}

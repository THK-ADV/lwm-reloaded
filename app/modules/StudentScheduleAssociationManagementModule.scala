package modules

import controllers.crud.StudentScheduleAssociationCRUDController

trait StudentScheduleAssociationManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def studentScheduleAssociationManagementController: StudentScheduleAssociationCRUDController
}

trait DefaultStudentScheduleAssociationManagementModuleImpl extends StudentScheduleAssociationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val studentScheduleAssociationManagementController: StudentScheduleAssociationCRUDController = new StudentScheduleAssociationCRUDController(repository, namespace, roleService)
}

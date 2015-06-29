package modules

import controllers.crud.StudentScheduleAssociationCRUDController

/**
 * Created by rgiacinto on 29/06/15.
 */
trait StudentScheduleAssociationManagementModule {
  self: SemanticRepositoryModule =>
  def studentScheduleAssociationManagementController: StudentScheduleAssociationCRUDController
}

trait DefaultStudentScheduleAssociationManagementModuleImpl extends StudentScheduleAssociationManagementModule {
  self: SemanticRepositoryModule =>
  lazy val studentScheduleAssociationManagementController: StudentScheduleAssociationCRUDController = new StudentScheduleAssociationCRUDController(repository, namespace)
}

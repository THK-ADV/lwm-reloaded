package modules

import controllers.crud.StudentCRUDController

/**
 * Created by rgiacinto on 29/06/15.
 */
trait StudentManagementModule {
  self: SemanticRepositoryModule =>
  def studentManagementController: StudentCRUDController
}

trait DefaultStudentManagementModuleImpl extends StudentManagementModule {
  self: SemanticRepositoryModule =>
  lazy val studentManagementController: StudentCRUDController = new StudentCRUDController(repository, namespace)
}
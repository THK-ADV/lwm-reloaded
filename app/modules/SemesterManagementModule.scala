package modules

import controllers.crud.SemesterCRUDController


trait SemesterManagementModule {
  self: SemanticRepositoryModule =>
  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule =>
  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, namespace)
}
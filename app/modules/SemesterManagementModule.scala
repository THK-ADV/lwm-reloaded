package modules

import controllers.crud.SemesterCRUDController


trait SemesterManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, namespace, roleService)
}

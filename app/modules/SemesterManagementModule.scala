package modules

import controllers.SemesterCRUDController


trait SemesterManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, sessionService, namespace, roleService)
}

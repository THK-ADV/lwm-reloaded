package modules

import controllers.crud.SemesterCRUDController


trait SemesterManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, namespace, roleService)
}

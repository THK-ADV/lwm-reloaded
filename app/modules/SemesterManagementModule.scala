package modules

import controllers.crud.SemesterCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}


trait SemesterManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, namespace, roleService)
}

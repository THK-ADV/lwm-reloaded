package modules.user

import controllers.crud.user.StudentCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait StudentManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def studentManagementController: StudentCRUDController
}

trait DefaultStudentManagementModuleImpl extends StudentManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val studentManagementController: StudentCRUDController = new StudentCRUDController(repository, namespace, roleService)
}

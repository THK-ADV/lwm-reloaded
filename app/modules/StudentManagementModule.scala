package modules

import controllers.crud.StudentCRUDController

trait StudentManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def studentManagementController: StudentCRUDController
}

trait DefaultStudentManagementModuleImpl extends StudentManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val studentManagementController: StudentCRUDController = new StudentCRUDController(repository, namespace, roleService)
}

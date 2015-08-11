package modules

import controllers.crud.EmployeeCRUDController


trait EmployeeManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def employeeManagementController: EmployeeCRUDController
}

trait DefaultEmployeeManagementModuleImpl extends EmployeeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val employeeManagementController: EmployeeCRUDController = new EmployeeCRUDController(repository, namespace, roleService)
}

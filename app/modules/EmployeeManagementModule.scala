package modules

import controllers.crud.EmployeeCRUDController


trait EmployeeManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def employeeManagementController: EmployeeCRUDController
}

trait DefaultEmployeeManagementModuleImpl extends EmployeeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val employeeManagementController: EmployeeCRUDController = new EmployeeCRUDController(repository, namespace, roleService)
}

package modules

import controllers.crud.EmployeeCRUDController


trait EmployeeManagementModule {
  self: SemanticRepositoryModule =>
  def employeeManagementController: EmployeeCRUDController
}

trait DefaultEmployeeManagementModuleImpl extends EmployeeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace =>
  lazy val employeeManagementController: EmployeeCRUDController = new EmployeeCRUDController(repository, namespace)
}

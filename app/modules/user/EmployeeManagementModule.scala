package modules.user

import controllers.crud.user.EmployeeCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait EmployeeManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def employeeManagementController: EmployeeCRUDController
}

trait DefaultEmployeeManagementModuleImpl extends EmployeeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val employeeManagementController: EmployeeCRUDController = new EmployeeCRUDController(repository, namespace, roleService)
}

package modules

import controllers.crud.DegreeCRUDController


trait DegreeManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, namespace, roleService)
}

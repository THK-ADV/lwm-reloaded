package modules

import controllers.crud.DegreeCRUDController


trait DegreeManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, namespace, roleService)
}

package modules

import controllers.crud.DegreeCRUDController


trait DegreeManagementModule {
  self: SemanticRepositoryModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace =>
  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, namespace)
}

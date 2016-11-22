package modules

import controllers.DegreeCRUDController

trait DegreeManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, sessionService, namespace, roleService)
}

package modules

import controllers.crud.DegreeCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}


trait DegreeManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, namespace, roleService)
}

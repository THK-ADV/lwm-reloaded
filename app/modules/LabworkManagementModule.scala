package modules

import controllers.crud.LabworkCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait LabworkManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def labworkManagementController: LabworkCRUDController
}


trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, namespace, roleService)
}

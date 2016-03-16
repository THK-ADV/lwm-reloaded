package modules

import controllers.crud.LabworkCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait LabworkManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>
  def labworkManagementController: LabworkCRUDController
}


trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>
  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, sessionService, namespace, roleService)
}

package modules

import controllers.crud.LabworkCRUDController


trait LabworkManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def labworkManagementController: LabworkCRUDController
}


trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, namespace, roleService)
}

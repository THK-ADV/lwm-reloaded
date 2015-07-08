package modules

import controllers.crud.LabworkCRUDController


trait LabworkManagementModule {
  self: SemanticRepositoryModule =>
  def labworkManagementController: LabworkCRUDController
}


trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule with BaseNamespace =>
  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, namespace)
}

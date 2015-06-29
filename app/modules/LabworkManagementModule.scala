package modules

import controllers.crud.LabworkCRUDController


trait LabworkManagementModule {
  self: SemanticRepositoryModule =>
  def labworkManagementController: LabworkCRUDController
}


trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule =>
  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, namespace)
}
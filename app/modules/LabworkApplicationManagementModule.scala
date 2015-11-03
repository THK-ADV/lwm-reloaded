package modules

import controllers.crud.LabworkApplicationCRUDController

trait LabworkApplicationManagementModule { self: SemanticRepositoryModule with SecurityManagementModule =>
  def labworkApplicationController: LabworkApplicationCRUDController
}

trait DefaultLabworkApplicationManagementModule extends LabworkApplicationManagementModule{
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  override def labworkApplicationController: LabworkApplicationCRUDController = new LabworkApplicationCRUDController(repository, namespace, roleService)
}

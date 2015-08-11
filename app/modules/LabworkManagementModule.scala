package modules

import controllers.crud.LabworkCRUDController


trait LabworkManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def labworkManagementController: LabworkCRUDController
}


trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, namespace, roleService)
}

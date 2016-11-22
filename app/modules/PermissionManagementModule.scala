package modules

import controllers.PermissionController

trait PermissionManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def permissionManagementController: PermissionController
}

trait DefaultPermissionManagementModule extends PermissionManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val permissionManagementController: PermissionController = new PermissionController(repository, sessionService, namespace, roleService)
}

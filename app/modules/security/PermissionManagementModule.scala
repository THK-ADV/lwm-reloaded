package modules.security

import controllers.PermissionController
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait PermissionManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def permissionManagementController: PermissionController
}

trait DefaultPermissionManagementModule extends PermissionManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  override def permissionManagementController: PermissionController = new PermissionController(repository, namespace, roleService)
}

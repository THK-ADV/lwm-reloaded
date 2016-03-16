package modules.security

import controllers.security.PermissionController
import modules.SessionRepositoryModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait PermissionManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def permissionManagementController: PermissionController
}

trait DefaultPermissionManagementModule extends PermissionManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override def permissionManagementController: PermissionController = new PermissionController(repository, sessionService, namespace, roleService)
}

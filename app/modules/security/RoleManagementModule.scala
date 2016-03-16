package modules.security

import controllers.security.RoleController
import modules.SessionRepositoryModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait RoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>
  def roleManagementController: RoleController
}

trait DefaultRoleManagementModule extends RoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>
  override def roleManagementController: RoleController = new RoleController(repository, sessionService, namespace, roleService)
}

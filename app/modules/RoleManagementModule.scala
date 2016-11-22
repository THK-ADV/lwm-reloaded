package modules

import controllers.RoleController

trait RoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def roleManagementController: RoleController
}

trait DefaultRoleManagementModule extends RoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val roleManagementController: RoleController = new RoleController(repository, sessionService, namespace, roleService)
}

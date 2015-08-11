package modules

import services.RoleService
import utils.LwmApplication

trait RoleManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>
  def roleService: RoleService
}

trait DefaultRoleManagementModuleImpl extends RoleManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>
  override def roleService: RoleService = new RoleService(repository)
}
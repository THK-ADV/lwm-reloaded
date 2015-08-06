package modules

import services.RoleService
import utils.LwmApplication

trait RoleManagementModule {
  self: LwmApplication =>
  def roleService: RoleService
}

trait DefaultRoleManagementModuleImpl extends RoleManagementModule {
  self: LwmApplication =>
  override def roleService: RoleService = new RoleService()
}
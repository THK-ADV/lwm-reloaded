package modules

import services.RoleService
import utils.LwmApplication

trait SecurityManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>

  def roleService: RoleService
}

trait DefaultSecurityManagementModule extends SecurityManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>

  lazy val roleService: RoleService = new RoleService(repository)
}
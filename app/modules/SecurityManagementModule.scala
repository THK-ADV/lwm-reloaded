package modules

import services.{RoleService, RoleServiceLike}
import utils.LwmApplication

trait SecurityManagementModule {
  self: LwmApplication with SemanticRepositoryModule with SessionRepositoryModule =>

  def roleService: RoleServiceLike
}

trait DefaultSecurityManagementModule extends SecurityManagementModule {
  self: LwmApplication with SemanticRepositoryModule with SessionRepositoryModule =>

  lazy val roleService: RoleService = new RoleService(repository)
}
package modules.security

import modules.SessionRepositoryModule
import modules.store.SemanticRepositoryModule
import services.RoleService
import utils.LwmApplication

trait SecurityManagementModule {
  self: LwmApplication with SemanticRepositoryModule with SessionRepositoryModule =>

  def roleService: RoleService
}

trait DefaultSecurityManagementModule extends SecurityManagementModule {
  self: LwmApplication with SemanticRepositoryModule with SessionRepositoryModule =>

  lazy val roleService: RoleService = new RoleService(repository)
}
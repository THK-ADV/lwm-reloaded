package modules

import controllers.AuthorityController
import services.{AuthorityService, AuthorityServiceImpl}

trait AuthorityManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def authorityManagementController: AuthorityController
}

trait DefaultAuthorityManagementModuleImpl extends AuthorityManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val authorityManagementController: AuthorityController = new AuthorityController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait AuthorityServiceModule {
  self: DatabaseModule with RoleServiceModule =>

  def authorityService: AuthorityService
}

trait DefaultAuthorityServiceModule extends AuthorityServiceModule {
  self: DatabaseModule with RoleServiceModule =>

  override lazy val authorityService = new AuthorityServiceImpl(db, roleService2)
}
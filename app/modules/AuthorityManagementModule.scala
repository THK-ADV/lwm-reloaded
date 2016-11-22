package modules

import controllers.AuthorityController

trait AuthorityManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def authorityManagementController: AuthorityController
}

trait DefaultAuthorityManagementModuleImpl extends AuthorityManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val authorityManagementController: AuthorityController = new AuthorityController(repository, sessionService, namespace, roleService)
}

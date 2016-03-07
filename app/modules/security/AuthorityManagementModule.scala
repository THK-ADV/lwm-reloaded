package modules.security

import controllers.security.AuthorityController
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait AuthorityManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def authorityManagementController: AuthorityController
}

trait DefaultAuthorityManagementModuleImpl extends AuthorityManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val authorityManagementController: AuthorityController = new AuthorityController(repository, namespace, roleService)
}

package modules.security

import controllers.crud.security.AuthorityCRUDController
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait AuthorityManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def authorityManagementController: AuthorityCRUDController
}

trait DefaultAuthorityManagementModuleImpl extends AuthorityManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val authorityManagementController: AuthorityCRUDController = new AuthorityCRUDController(repository, namespace, roleService)
}

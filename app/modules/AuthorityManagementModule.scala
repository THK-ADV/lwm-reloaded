package modules

import controllers.crud.AuthorityCRUDController

trait AuthorityManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>

  def authorityManagementController: AuthorityCRUDController
}

trait DefaultAuthorityManagementModuleImpl extends AuthorityManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>

  lazy val authorityManagementController: AuthorityCRUDController = new AuthorityCRUDController(repository, namespace, roleService)
}

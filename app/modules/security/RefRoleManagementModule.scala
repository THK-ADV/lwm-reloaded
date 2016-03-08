package modules.security

import controllers.security.RefRoleController
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait RefRoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def refRoleManagementController: RefRoleController
}

trait DefaultRefRoleManagementModuleImpl extends RefRoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val refRoleManagementController: RefRoleController = new RefRoleController(repository, namespace, roleService)
}
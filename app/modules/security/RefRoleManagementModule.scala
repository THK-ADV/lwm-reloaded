package modules.security

import controllers.security.RefRoleController
import modules.SessionRepositoryModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait RefRoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule=>

  def refRoleManagementController: RefRoleController
}

trait DefaultRefRoleManagementModuleImpl extends RefRoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val refRoleManagementController: RefRoleController = new RefRoleController(repository, sessionService, namespace, roleService)
}
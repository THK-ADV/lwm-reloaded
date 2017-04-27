package modules

import controllers.RoleController
import services.{RoleService2, RoleServiceImpl}

trait RoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def roleManagementController: RoleController
}

trait DefaultRoleManagementModule extends RoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val roleManagementController: RoleController = new RoleController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait RoleServiceModule { self: DatabaseModule =>
  def roleService2: RoleService2
}

trait DefaultRoleServiceModule extends RoleServiceModule { self: DatabaseModule =>
  override lazy val roleService2 = new RoleServiceImpl(db)
}
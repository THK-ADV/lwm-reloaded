package modules

import controllers.{RoleController, RoleControllerPostgres}
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

trait RoleManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  def roleManagementControllerPostgres: RoleControllerPostgres
}

trait DefaultRoleManagementModulePostgres extends RoleManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with RoleServiceModule=>

  override lazy val roleManagementControllerPostgres: RoleControllerPostgres = new RoleControllerPostgres(sessionService, roleService2, roleService)
}
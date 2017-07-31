package modules

import controllers.{RoleController, RoleControllerPostgres}
import dao.{RoleDao, RoleDaoImpl}

trait RoleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def roleManagementController: RoleController
}

trait DefaultRoleManagementModule extends RoleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val roleManagementController: RoleController = new RoleController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait RoleDaoModule { self: DatabaseModule =>
  def roleDao: RoleDao
}

trait DefaultRoleDaoModule extends RoleDaoModule { self: DatabaseModule =>
  override lazy val roleDao = new RoleDaoImpl(db)
}

trait RoleManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  def roleManagementControllerPostgres: RoleControllerPostgres
}

trait DefaultRoleManagementModulePostgres extends RoleManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with RoleDaoModule=>

  override lazy val roleManagementControllerPostgres: RoleControllerPostgres = new RoleControllerPostgres(sessionService, roleDao, roleService)
}
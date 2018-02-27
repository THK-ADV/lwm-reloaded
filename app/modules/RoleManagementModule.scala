package modules

import controllers.RoleControllerPostgres
import dao.{RoleDao, RoleDaoImpl}

trait RoleDaoModule {
  self: DatabaseModule =>

  def roleDao: RoleDao
}

trait DefaultRoleDaoModule extends RoleDaoModule {
  self: DatabaseModule =>

  override lazy val roleDao = new RoleDaoImpl(db)
}

trait RoleManagementModulePostgres {
  self: AuthorityDaoModule with RoleDaoModule with SessionRepositoryModule =>

  def roleManagementControllerPostgres: RoleControllerPostgres
}

trait DefaultRoleManagementModulePostgres extends RoleManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with RoleDaoModule =>

  override lazy val roleManagementControllerPostgres: RoleControllerPostgres = new RoleControllerPostgres(sessionService, roleDao, authorityDao)
}
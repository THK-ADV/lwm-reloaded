package modules

import controllers.{PermissionController, PermissionControllerPostgres}
import dao.{PermissionDao, PermissionDaoImpl}
import utils.LwmApplication

trait PermissionManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def permissionManagementController: PermissionController
}

trait DefaultPermissionManagementModule extends PermissionManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val permissionManagementController: PermissionController = new PermissionController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait PermissionDaoModule { self: DatabaseModule =>
  def permissionDao: PermissionDao
}

trait DefaultPermissionDaoModule extends PermissionDaoModule { self: DatabaseModule =>
  override lazy val permissionDao = new PermissionDaoImpl(db)
}

trait PermissionManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with PermissionDaoModule =>

  def permissionControllerPostgres: PermissionControllerPostgres
}

trait DefaultPermissionManagementModule2 extends PermissionManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with PermissionDaoModule =>

  override lazy val permissionControllerPostgres = new PermissionControllerPostgres(authorityDao, sessionService, permissionDao)
}
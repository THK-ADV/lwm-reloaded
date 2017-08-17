package modules

import controllers.{AuthorityController, AuthorityControllerPostgres}
import dao.{AuthorityDao, AuthorityDaoImpl}

trait AuthorityManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def authorityManagementController: AuthorityController
}

trait DefaultAuthorityManagementModuleImpl extends AuthorityManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val authorityManagementController: AuthorityController = new AuthorityController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait AuthorityDaoModule {
  self: DatabaseModule with RoleDaoModule =>

  def authorityDao: AuthorityDao
}

trait DefaultAuthorityDaoModule extends AuthorityDaoModule {
  self: DatabaseModule with RoleDaoModule =>

  override lazy val authorityDao = new AuthorityDaoImpl(db, roleDao)
}

trait AuthorityManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule =>

  def authorityControllerPostgres: AuthorityControllerPostgres
}

trait DefaultAuthorityManagementModule2 extends AuthorityManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule =>

  override lazy val authorityControllerPostgres: AuthorityControllerPostgres = new AuthorityControllerPostgres(authorityDao, sessionService)
}
package modules

import controllers.AuthorityController
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
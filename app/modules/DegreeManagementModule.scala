package modules

import controllers.{DegreeCRUDController, DegreeControllerPostgres}
import dao.{DegreeDao, DegreeDaoImpl}

trait DegreeManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait DegreeDaoModule { self: DatabaseModule =>
  def degreeDao: DegreeDao
}

trait DefaultDegreeDaoModule extends DegreeDaoModule { self: DatabaseModule =>
  override lazy val degreeDao = new DegreeDaoImpl(db)
}

trait DegreeManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with DegreeDaoModule =>

  def degreeManagementControllerPostgres: DegreeControllerPostgres
}

trait DefaultDegreeManagementModuleImplPostgres extends DegreeManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with DegreeDaoModule =>

  lazy val degreeManagementControllerPostgres: DegreeControllerPostgres = new DegreeControllerPostgres(sessionService, authorityDao, degreeDao)
}

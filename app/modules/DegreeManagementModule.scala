package modules

import controllers.DegreeControllerPostgres
import dao.{DegreeDao, DegreeDaoImpl}

trait DegreeDaoModule {
  self: DatabaseModule =>

  def degreeDao: DegreeDao
}

trait DefaultDegreeDaoModule extends DegreeDaoModule {
  self: DatabaseModule =>

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

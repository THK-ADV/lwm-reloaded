package modules

import controllers.LabworkControllerPostgres
import dao.{LabworkDao, LabworkDaoImpl}

trait LabworkDaoModule {
  self: DatabaseModule =>

  def labworkDao: LabworkDao
}

trait DefaultLabworkDaoModule extends LabworkDaoModule {
  self: DatabaseModule =>

  override lazy val labworkDao = new LabworkDaoImpl(db)
}

trait LabworkManagementModulePostgres {
  def labworControllerPostgres: LabworkControllerPostgres
}

trait DefaultLabworkManagementModulePostgres extends LabworkManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with LabworkDaoModule =>

  override lazy val labworControllerPostgres: LabworkControllerPostgres = new LabworkControllerPostgres(sessionService, authorityDao, labworkDao)
}
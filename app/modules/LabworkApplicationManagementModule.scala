package modules

import controllers.LabworkApplicationControllerPostgres
import dao.{LabworkApplicationDao, LabworkApplicationDaoImpl}


trait LabworkApplicationDaoModule {
  self: DatabaseModule =>

  def labworkApplicationDao: LabworkApplicationDao
}

trait DefaultLabworkApplicationDaoModule extends LabworkApplicationDaoModule {
  self: DatabaseModule =>

  override lazy val labworkApplicationDao = new LabworkApplicationDaoImpl(db)
}

trait LabworkApplicationManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with LabworkApplicationDaoModule =>

  def labworkApplicationControllerPostgres: LabworkApplicationControllerPostgres
}

trait DefaultLabworkApplicationManagementModulePostgres extends LabworkApplicationManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with LabworkApplicationDaoModule =>

  override lazy val labworkApplicationControllerPostgres: LabworkApplicationControllerPostgres = new LabworkApplicationControllerPostgres(sessionService, authorityDao, labworkApplicationDao)
}
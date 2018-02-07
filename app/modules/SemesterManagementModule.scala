package modules

import controllers.SemesterControllerPostgres
import dao.{SemesterDao, SemesterDaoImpl}

trait SemesterDaoModule {
  self: DatabaseModule =>

  def semesterDao: SemesterDao
}

trait DefaultSemesterDaoModule extends SemesterDaoModule {
  self: DatabaseModule =>

  override lazy val semesterDao = new SemesterDaoImpl(db)
}

trait SemesterManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with SemesterDaoModule =>

  def semesterManagementControllerPostgres: SemesterControllerPostgres
}

trait DefaultSemesterManagementModuleImplPostgres extends SemesterManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with SemesterDaoModule =>

  lazy val semesterManagementControllerPostgres: SemesterControllerPostgres = new SemesterControllerPostgres(sessionService, authorityDao, semesterDao)
}
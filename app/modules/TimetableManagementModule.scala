package modules

import controllers.TimetableControllerPostgres
import dao.{TimetableDao, TimetableDaoImpl}

trait TimetableDaoManagementModule {
  self: DatabaseModule =>

  def timetableDao: TimetableDao
}

trait DefaultTimetableDaoModule extends TimetableDaoManagementModule {
  self: DatabaseModule =>

  override lazy val timetableDao: TimetableDao = new TimetableDaoImpl(db)
}

trait TimetableManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with TimetableDaoManagementModule =>

  def timetableControllerPostgres: TimetableControllerPostgres
}

trait DefaultTimetableManagementModulePostgres extends TimetableManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with TimetableDaoManagementModule =>

  override lazy val timetableControllerPostgres = new TimetableControllerPostgres(authorityDao, sessionService, timetableDao)
}
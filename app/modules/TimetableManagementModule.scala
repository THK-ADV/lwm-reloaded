package modules

import controllers.{TimetableCRUDController, TimetableControllerPostgres}
import dao.{TimetableDao, TimetableDaoImpl}

trait TimetableManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def timetableManagementController: TimetableCRUDController
}

trait DefaultTimetableManagementModuleImpl extends TimetableManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val timetableManagementController: TimetableCRUDController = new TimetableCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait TimetableDaoManagementModule { self: DatabaseModule =>
  def timetableDao: TimetableDao
}

trait DefaultTimetableDaoModule extends TimetableDaoManagementModule { self: DatabaseModule =>
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
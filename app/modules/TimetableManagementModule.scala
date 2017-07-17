package modules

import controllers.{TimetableCRUDController, TimetableControllerPostgres}
import services.{TimetableService, TimetableService2, TimetableService2Impl, TimetableServiceLike}
import utils.LwmApplication

trait TimetableServiceManagementModule {
  self: LwmApplication with BlacklistServiceManagementModule =>

  def timetableService: TimetableServiceLike
}

trait DefaultTimetableServiceManagementModule extends TimetableServiceManagementModule {
  self: LwmApplication with BlacklistServiceManagementModule =>

  lazy val timetableService: TimetableServiceLike = new TimetableService(blacklistService)
}

trait TimetableManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def timetableManagementController: TimetableCRUDController
}

trait DefaultTimetableManagementModuleImpl extends TimetableManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val timetableManagementController: TimetableCRUDController = new TimetableCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait TimetableService2ManagementModule { self: DatabaseModule =>

  def timetableService2: TimetableService2
}

trait DefaultTimetableService2Module extends TimetableService2ManagementModule { self: DatabaseModule =>

  override lazy val timetableService2: TimetableService2 = new TimetableService2Impl(db)
}

trait TimetableManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with TimetableService2ManagementModule =>

  def timetableControllerPostgres: TimetableControllerPostgres
}

trait DefaultTimetableManagementModulePostgres extends TimetableManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with TimetableService2ManagementModule =>

  override lazy val timetableControllerPostgres = new TimetableControllerPostgres(roleService, sessionService, timetableService2)
}
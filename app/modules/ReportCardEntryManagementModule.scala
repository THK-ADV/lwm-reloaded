package modules

import controllers.{ReportCardEntryController, ReportCardEntryControllerPostgres}
import dao.{ReportCardEntryDao, ReportCardEntryDaoImpl}

trait ReportCardEntryManagementModule {

  def reportCardEntryManagementController: ReportCardEntryController
}

trait DefaultReportCardEntryManagementModuleImpl extends ReportCardEntryManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with ReportCardServiceManagementModule =>

  lazy val reportCardEntryManagementController: ReportCardEntryController = new ReportCardEntryController(repository, sessionService, namespace, roleService, reportCardService)
}

// POSTGRES

trait ReportCardEntryDaoModule { self: DatabaseModule =>
  def reportCardEntryDao: ReportCardEntryDao
}

trait DefaultReportCardEntryDaoModule extends ReportCardEntryDaoModule { self: DatabaseModule =>
  override lazy val reportCardEntryDao = new ReportCardEntryDaoImpl(db)
}

trait ReportCardEntryManagementModule2 {
  self: SecurityManagementModule with SessionRepositoryModule with ReportCardEntryDaoModule with ScheduleEntryDaoModule with AssignmentPlanDaoModule =>

  def reportCardEntryControllerPostgres: ReportCardEntryControllerPostgres
}

trait DefaultReportCardEntryManagementModule2 extends ReportCardEntryManagementModule2 {
  self: SecurityManagementModule with SessionRepositoryModule with ReportCardEntryDaoModule with ScheduleEntryDaoModule with AssignmentPlanDaoModule =>

  override lazy val reportCardEntryControllerPostgres = new ReportCardEntryControllerPostgres(sessionService, roleService, reportCardEntryDao, scheduleEntryDao, assignmentPlanDao)
}
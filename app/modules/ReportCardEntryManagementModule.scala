package modules

import controllers.ReportCardEntryControllerPostgres
import dao.{ReportCardEntryDao, ReportCardEntryDaoImpl}

trait ReportCardEntryDaoModule {
  self: DatabaseModule =>

  def reportCardEntryDao: ReportCardEntryDao
}

trait DefaultReportCardEntryDaoModule extends ReportCardEntryDaoModule {
  self: DatabaseModule =>

  override lazy val reportCardEntryDao = new ReportCardEntryDaoImpl(db)
}

trait ReportCardEntryManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEntryDaoModule with ScheduleEntryDaoModule with AssignmentPlanDaoModule =>

  def reportCardEntryControllerPostgres: ReportCardEntryControllerPostgres
}

trait DefaultReportCardEntryManagementModule2 extends ReportCardEntryManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEntryDaoModule with ScheduleEntryDaoModule with AssignmentPlanDaoModule =>

  override lazy val reportCardEntryControllerPostgres = new ReportCardEntryControllerPostgres(sessionService, authorityDao, reportCardEntryDao, scheduleEntryDao, assignmentPlanDao)
}
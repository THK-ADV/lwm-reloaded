package modules

import controllers.ReportCardRescheduledController
import dao.{ReportCardRescheduledDao, ReportCardRescheduledImpl}

trait ReportCardRescheduledDaoModule {
  self: DatabaseModule =>

  def reportCardRescheduledDao: ReportCardRescheduledDao
}

trait DefaultReportCardRescheduledDaoModule extends ReportCardRescheduledDaoModule {
  self: DatabaseModule =>

  override lazy val reportCardRescheduledDao = new ReportCardRescheduledImpl(db)
}

trait ReportCardRescheduledManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardRescheduledDaoModule =>

  def reportCardRescheduledController: ReportCardRescheduledController
}

trait DefaultReportCardRescheduledManagementModule extends ReportCardRescheduledManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardRescheduledDaoModule =>

  override lazy val reportCardRescheduledController = new ReportCardRescheduledController(sessionService, authorityDao, reportCardRescheduledDao)
}
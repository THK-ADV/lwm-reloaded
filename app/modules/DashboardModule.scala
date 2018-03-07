package modules

import controllers.DashboardController
import dao.{DashboardDao, DashboardDaoImpl}

trait DashboardModule {
  self: DatabaseModule with LabworkDaoModule with LabworkApplicationDaoModule with GroupDaoManagementModule
    with ReportCardEntryDaoModule with ReportCardEvaluationDaoModule with ReportCardEvaluationPatternDaoModule =>

  def dashboardDao: DashboardDao
}

trait DefaultDashboardModule extends DashboardModule {
  self: DatabaseModule with LabworkDaoModule with LabworkApplicationDaoModule with GroupDaoManagementModule
    with ReportCardEntryDaoModule with ReportCardEvaluationDaoModule with ReportCardEvaluationPatternDaoModule =>

  lazy val dashboardDao = new DashboardDaoImpl(db, labworkDao, labworkApplicationDao, reportCardEntryDao, reportCardEvaluationDao, reportCardEvaluationPatternDao, groupDao)
}

trait DashboardManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with DashboardModule =>

  def dashboardController: DashboardController
}

trait DefaultDashboardManagementModule extends DashboardManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with DashboardModule =>

  lazy val dashboardController = new DashboardController(authorityDao, sessionService, dashboardDao)
}
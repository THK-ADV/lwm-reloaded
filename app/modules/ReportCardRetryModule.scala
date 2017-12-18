package modules

import controllers.ReportCardRetryController
import dao.{ReportCardRetryDao, ReportCardRetryDaoImpl}

trait ReportCardRetryDaoModule { self: DatabaseModule =>
  def reportCardRetryDao: ReportCardRetryDao
}

trait DefaultReportCardRetryDaoModule extends ReportCardRetryDaoModule { self: DatabaseModule =>
  override lazy val reportCardRetryDao = new ReportCardRetryDaoImpl(db)
}

trait ReportCardRetryManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardRetryDaoModule =>

  def reportCardRetryController: ReportCardRetryController
}

trait DefaultReportCardRetryManagementModule extends ReportCardRetryManagementModule {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardRetryDaoModule =>

  override lazy val reportCardRetryController = new ReportCardRetryController(sessionService, authorityDao, reportCardRetryDao)
}
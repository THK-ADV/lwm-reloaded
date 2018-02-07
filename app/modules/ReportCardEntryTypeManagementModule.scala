package modules

import controllers.ReportCardEntryTypeControllerPostgres
import dao.{ReportCardEntryTypeDao, ReportCardEntryTypeDaoImpl}

trait ReportCardEntryTypeDaoModule {
  self: DatabaseModule =>

  def reportCardEntryTypeDao: ReportCardEntryTypeDao
}

trait DefaultReportCardEntryTypeDaoModule extends ReportCardEntryTypeDaoModule {
  self: DatabaseModule =>

  override lazy val reportCardEntryTypeDao = new ReportCardEntryTypeDaoImpl(db)
}

trait ReportCardEntryTypeManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEntryTypeDaoModule =>

  def reportCardEntryTypeController: ReportCardEntryTypeControllerPostgres
}

trait DefaultReportCardEntryTypeManagementModule2 extends ReportCardEntryTypeManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEntryTypeDaoModule =>

  override lazy val reportCardEntryTypeController = new ReportCardEntryTypeControllerPostgres(authorityDao, sessionService, reportCardEntryTypeDao)
}
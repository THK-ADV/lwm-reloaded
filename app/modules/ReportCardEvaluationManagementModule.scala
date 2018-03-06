package modules

import controllers.ReportCardEvaluationControllerPostgres
import dao.{ReportCardEvaluationDao, ReportCardEvaluationDaoImpl}

trait ReportCardEvaluationDaoModule {
  self: DatabaseModule =>

  def reportCardEvaluationDao: ReportCardEvaluationDao
}

trait DefaultReportCardEvaluationDaoModule extends ReportCardEvaluationDaoModule {
  self: DatabaseModule =>

  override lazy val reportCardEvaluationDao = new ReportCardEvaluationDaoImpl(db)
}

trait ReportCardEvaluationManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEvaluationDaoModule with ReportCardEntryDaoModule with ReportCardEvaluationPatternDaoModule =>

  def reportCardEvaluationManagementController2: ReportCardEvaluationControllerPostgres
}

trait DefaultReportCardEvaluationManagementModule2 extends ReportCardEvaluationManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEvaluationDaoModule with ReportCardEntryDaoModule with ReportCardEvaluationPatternDaoModule =>

  override lazy val reportCardEvaluationManagementController2 = new ReportCardEvaluationControllerPostgres(authorityDao, sessionService, reportCardEvaluationDao, reportCardEntryDao, reportCardEvaluationPatternDao)
}
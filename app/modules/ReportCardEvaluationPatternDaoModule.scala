package modules

import controllers.ReportCardEvaluationPatternController
import dao.{ReportCardEvaluationPatternDao, ReportCardEvaluationPatternDaoImpl}

trait ReportCardEvaluationPatternDaoModule {
  self: DatabaseModule =>

  def reportCardEvaluationPatternDao: ReportCardEvaluationPatternDao
}

trait DefaultReportCardEvaluationPatternDaoModule {
  self: DatabaseModule =>

  lazy val reportCardEvaluationPatternDao = new ReportCardEvaluationPatternDaoImpl(db)
}

trait ReportCardEvaluationPatternModule {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEvaluationPatternDaoModule =>

  def reportCardEvaluationPatternController: ReportCardEvaluationPatternController
}

trait DefaultReportCardEvaluationPatternModule {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEvaluationPatternDaoModule =>

  lazy val reportCardEvaluationPatternController = new ReportCardEvaluationPatternController(authorityDao, sessionService, reportCardEvaluationPatternDao)
}
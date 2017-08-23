package modules

import controllers.{ReportCardEvaluationController, ReportCardEvaluationControllerPostgres}
import dao.{ReportCardEvaluationDao, ReportCardEvaluationDaoImpl}

trait ReportCardEvaluationManagementModule {

  def reportCardEvaluationManagementController: ReportCardEvaluationController
}

trait DefaultReportCardEvaluationManagementModuleImpl extends ReportCardEvaluationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with ReportCardServiceManagementModule =>

  lazy val reportCardEvaluationManagementController: ReportCardEvaluationController = new ReportCardEvaluationController(repository, sessionService, namespace, roleService, reportCardService)
}

// POSTGRES

trait ReportCardEvaluationDaoModule { self: DatabaseModule =>
  def reportCardEvaluationDao: ReportCardEvaluationDao
}

trait DefaultReportCardEvaluationDaoModule extends ReportCardEvaluationDaoModule { self: DatabaseModule =>
  override lazy val reportCardEvaluationDao = new ReportCardEvaluationDaoImpl(db)
}

trait ReportCardEvaluationManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEvaluationDaoModule with ReportCardEntryDaoModule =>

  def reportCardEvaluationManagementController2: ReportCardEvaluationControllerPostgres
}

trait DefaultReportCardEvaluationManagementModule2 extends ReportCardEvaluationManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEvaluationDaoModule with ReportCardEntryDaoModule =>

  override lazy val reportCardEvaluationManagementController2 = new ReportCardEvaluationControllerPostgres(authorityDao, sessionService, reportCardEvaluationDao, reportCardEntryDao)
}
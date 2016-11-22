package modules

import controllers.ReportCardEvaluationController

trait ReportCardEvaluationManagementModule {

  def reportCardEvaluationManagementController: ReportCardEvaluationController
}

trait DefaultReportCardEvaluationManagementModuleImpl extends ReportCardEvaluationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with ReportCardServiceManagementModule =>

  lazy val reportCardEvaluationManagementController: ReportCardEvaluationController = new ReportCardEvaluationController(repository, sessionService, namespace, roleService, reportCardService)
}

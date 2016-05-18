package modules.labwork.reportCard

import controllers.reportCard.ReportCardEvaluationController
import modules.SessionRepositoryModule
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait ReportCardEvaluationManagementModule {

  def reportCardEvaluationManagementController: ReportCardEvaluationController
}

trait DefaultReportCardEvaluationManagementModuleImpl extends ReportCardEvaluationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with ReportCardServiceManagementModule =>

  lazy val reportCardEvaluationManagementController: ReportCardEvaluationController = new ReportCardEvaluationController(repository, sessionService, namespace, roleService, reportCardService)
}

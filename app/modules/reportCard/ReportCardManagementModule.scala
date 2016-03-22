package modules.reportCard

import controllers.reportCard.ReportCardController
import modules.SessionRepositoryModule
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait ReportCardManagementModule {

  def reportCardManagementController: ReportCardController
}

trait DefaultReportCardManagementModuleImpl extends ReportCardManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val reportCardManagementController: ReportCardController = new ReportCardController(repository, sessionService, namespace, roleService)
}
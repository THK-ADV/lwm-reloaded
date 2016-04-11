package modules.labwork.reportCard

import controllers.reportCard.ReportCardEntryController
import modules.SessionRepositoryModule
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait ReportCardEntryManagementModule {

  def reportCardEntryManagementController: ReportCardEntryController
}

trait DefaultReportCardEntryManagementModuleImpl extends ReportCardEntryManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with ReportCardServiceManagementModule =>

  lazy val reportCardEntryManagementController: ReportCardEntryController = new ReportCardEntryController(repository, sessionService, namespace, roleService, reportCardService)
}

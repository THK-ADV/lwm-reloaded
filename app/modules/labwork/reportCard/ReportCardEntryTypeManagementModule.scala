package modules.labwork.reportCard

import controllers.reportCard.ReportCardEntryTypeController
import modules.SessionRepositoryModule
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait ReportCardEntryTypeManagementModule {

  def reportCardEntryTypeManagementController: ReportCardEntryTypeController
}

trait DefaultReportCardEntryTypeManagementModuleImpl extends ReportCardEntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val reportCardEntryTypeManagementController: ReportCardEntryTypeController = new ReportCardEntryTypeController(repository, sessionService, namespace, roleService)
}

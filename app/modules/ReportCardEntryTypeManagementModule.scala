package modules

import controllers.ReportCardEntryTypeController

trait ReportCardEntryTypeManagementModule {

  def reportCardEntryTypeManagementController: ReportCardEntryTypeController
}

trait DefaultReportCardEntryTypeManagementModuleImpl extends ReportCardEntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val reportCardEntryTypeManagementController: ReportCardEntryTypeController = new ReportCardEntryTypeController(repository, sessionService, namespace, roleService)
}

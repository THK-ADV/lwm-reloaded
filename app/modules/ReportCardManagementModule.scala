package modules

import controllers.ReportCardController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services.{ReportCardService, ReportCardServiceLike}
import utils.LwmApplication

trait ReportCardServiceManagementModule {
  self: LwmApplication =>

  def reportCardService: ReportCardServiceLike
}

trait DefaultReportCardServiceManagementModule extends ReportCardServiceManagementModule {
  self: LwmApplication =>

  lazy val reportCardService: ReportCardServiceLike = new ReportCardService
}

trait ReportCardManagementModule {
  self: SemanticRepositoryModule  =>

  def reportCardManagementController: ReportCardController
}

trait DefaultReportCardManagementModuleImpl extends ReportCardManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val reportCardManagementController: ReportCardController = new ReportCardController(repository, namespace, roleService)
}
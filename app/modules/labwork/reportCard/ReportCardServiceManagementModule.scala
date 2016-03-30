package modules.labwork.reportCard

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

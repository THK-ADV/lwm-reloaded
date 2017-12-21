package modules

import services._
import utils.LwmApplication

trait ScheduleServiceManagementModule {
  self: LwmApplication =>

  def scheduleService: ScheduleGenesisServiceLike2
}

trait DefaultScheduleServiceManagementModule extends ScheduleServiceManagementModule {
  self: LwmApplication =>

  lazy val scheduleService: ScheduleGenesisServiceLike2 = new ScheduleService2(20, 100, 10)
}
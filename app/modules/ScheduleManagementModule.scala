package modules

import services._

trait ScheduleServiceManagementModule {
  def scheduleService: ScheduleGenesisServiceLike
}

trait DefaultScheduleServiceManagementModule extends ScheduleServiceManagementModule {
  self: ConfigurationModule =>

  val populations = lwmConfig.getInt("lwm.schedule.populations").getOrElse(20)
  val generations = lwmConfig.getInt("lwm.schedule.generations").getOrElse(100)
  val elites = lwmConfig.getInt("lwm.schedule.elites").getOrElse(10)

  lazy val scheduleService: ScheduleGenesisServiceLike = new ScheduleService(populations, generations, elites)
}
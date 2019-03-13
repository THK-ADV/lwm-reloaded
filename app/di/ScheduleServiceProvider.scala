package di

import javax.inject.{Inject, Provider, Singleton}
import play.api.Configuration
import services.{ScheduleService, ScheduleServiceImpl}

@Singleton
class ScheduleServiceProvider @Inject()(config: Configuration) extends Provider[ScheduleService] {
  lazy val get = {
    val pops = config getOptional[Int] "lwm.schedule.populations" getOrElse 20
    val gens = config getOptional[Int] "lwm.schedule.generations" getOrElse 100
    val elites = config getOptional[Int] "lwm.schedule.elites" getOrElse 10

    new ScheduleServiceImpl(pops, gens, elites)
  }
}

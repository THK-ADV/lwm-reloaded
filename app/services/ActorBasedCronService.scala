package services

import akka.actor.{ActorRef, ActorSystem, Props}
import us.theatr.akka.quartz.{AddCronSchedule, QuartzActor}

case class CronJob(expression: String, actorRef: ActorRef, message: Any)

trait CronService {
  def cronJobs: List[CronJob]
}

final class ActorBasedCronService(val system: ActorSystem, val cronJobs: List[CronJob]) extends CronService {

  lazy val quartzActor = system.actorOf(Props[QuartzActor])

  cronJobs.foreach { job =>
    quartzActor ! AddCronSchedule(job.actorRef, job.expression, job.message)
  }
}
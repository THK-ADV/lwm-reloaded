package services

import java.time.LocalTime

import akka.actor.{ActorRef, ActorSystem}
import di.BackupServiceActorAnnotation
import javax.inject.{Inject, Named}

import scala.util.Try

class ActorScheduler @Inject()(
  system: ActorSystem,
  @BackupServiceActorAnnotation ref1: ActorRef,
  @Named("backupMessage") message1: Any,
  @Named("backupFireTime") fireTime1: String
) {
  import ActorScheduler._

  val s = schedule(system)(_, _, _)

  List((ref1, message1, fireTime1))
    .map(a => (a._1, a._2, parse(a._3)))
    .filter(_._3.isSuccess)
    .foreach(a => s(a._1, a._2, a._3.get))
}

object ActorScheduler {

  import scala.concurrent.duration._

  implicit val interval: FiniteDuration = 24.hours

  /**
    * calculates delay in seconds between `now` and `fireTime` time concerning `interval` boundaries.
    * `fireTime` and `now` needs to be ISO-8601 time formatted such as "13:37:00".
    *
    * @param fireTime destination time to fire
    * @param now      current time for calculation purpose
    * @param interval of schedule
    * @return delay in seconds
    */
  def delayUntil(fireTime: LocalTime, now: LocalTime = LocalTime.now)(implicit interval: FiniteDuration): FiniteDuration = {
    val t = fireTime.toSecondOfDay
    val n = now.toSecondOfDay
    val diff = t - n

    val delay = if (diff < 0) interval.toSeconds + diff else diff
    delay.seconds
  }

  def parse(time: String): Try[LocalTime] = Try(LocalTime.parse(time))

  def schedule(system: ActorSystem)(ref: ActorRef, message: Any, fireTime: LocalTime): Unit = {
    system.scheduler.schedule(delayUntil(fireTime), interval, ref, message)(system.dispatcher)
  }
}
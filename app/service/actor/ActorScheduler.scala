package service.actor

import java.time.LocalTime

import akka.actor.{ActorRef, ActorSystem}
import di.{BackupServiceActorAnnotation, SemesterCreationActorAnnotation}
import javax.inject.{Inject, Named}

import scala.util.{Success, Try}

final class ActorScheduler @Inject()(
  system: ActorSystem,
  @BackupServiceActorAnnotation backupServiceActor: ActorRef,
  @Named("backupMessage") backupMessage: Any,
  @Named("backupFireTime") backupFireTime: String,
  @SemesterCreationActorAnnotation semesterCreationActor: ActorRef,
  @Named("semesterCreationMessage") semesterCreationMessage: Any,
  @Named("semesterCreationFireTime") semesterCreationFireTime: String
) {

  import ActorScheduler._

  private def actors: List[Try[ScheduledActor]] = List(
    ScheduledActor(backupServiceActor, backupMessage, backupFireTime),
    ScheduledActor(semesterCreationActor, semesterCreationMessage, semesterCreationFireTime),
  )

  actors
    .collect { case Success(s) => s }
    .foreach(actor => schedule(system, actor))
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

  def schedule(system: ActorSystem, scheduled: ScheduledActor): Unit = {
    system.scheduler.schedule(delayUntil(scheduled.fireTime), interval, scheduled.actor, scheduled.message)(system.dispatcher)
  }
}
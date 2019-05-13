package service.actor

import java.time.LocalTime

import akka.actor.ActorRef

import scala.util.Try

trait ScheduledActor {
  def actor: ActorRef

  def message: Any

  def fireTime: LocalTime
}

object ScheduledActor {

  private case class Instance(actor: ActorRef, message: Any, fireTime: LocalTime) extends ScheduledActor

  def apply(actor: ActorRef, message: Any, fireTime: String): Try[ScheduledActor] = {
    parse(fireTime).map(t => Instance(actor, message, t))
  }

  def parse(time: String): Try[LocalTime] = Try(LocalTime.parse(time))
}
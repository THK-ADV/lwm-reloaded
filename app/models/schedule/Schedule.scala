package models.schedule

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import play.api.libs.json.{Json, Reads, Writes}

case class Schedule(labwork: UUID, id: UUID) extends UniqueEntity

case class ScheduleProtocol(labwork: UUID)

object Schedule extends UriGenerator[Schedule] with JsonSerialisation[ScheduleProtocol, Schedule] {

  override def base: String = "schedules"

  override implicit def reads: Reads[ScheduleProtocol] = Json.reads[ScheduleProtocol]

  override implicit def writes: Writes[Schedule] = Json.writes[Schedule]
}
package models.schedules

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class StudentSchedule(id: Option[UUID] = Some(UUID.randomUUID())) extends UniqueEntity

// not included associations: []

object StudentSchedule extends UriGenerator[StudentSchedule] with JsonSerialisation[StudentSchedule] {

  override implicit def reads: Reads[StudentSchedule] = Json.reads[StudentSchedule]

  override implicit def writes: Writes[StudentSchedule] = Json.writes[StudentSchedule]

  override def base: String = "studentSchedules"
}
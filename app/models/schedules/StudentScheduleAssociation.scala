package models.schedules

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class StudentScheduleAssociation(date: String, groupScheduleAssociation: String, timetableEntry: String, id: Option[UUID] = Some(UUID.randomUUID())) extends UniqueEntity

object StudentScheduleAssociation extends UriGenerator[StudentScheduleAssociation] with JsonSerialisation[StudentScheduleAssociation] {

  override implicit def reads: Reads[StudentScheduleAssociation] = Json.reads[StudentScheduleAssociation]

  override implicit def writes: Writes[StudentScheduleAssociation] = Json.writes[StudentScheduleAssociation]

  override def base: String = "studentScheduleAssociations"
}
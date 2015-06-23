package models.schedules

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}

case class StudentScheduleAssociation(date: String, groupScheduleAssociation: String, timetableEntry: String, id: UUID) extends UniqueEntity

case class StudentScheduleAssociationProtocol(date: String, groupScheduleAssociation: String, timetableEntry: String)

object StudentScheduleAssociation extends UriGenerator[StudentScheduleAssociation] with JsonSerialisation[StudentScheduleAssociationProtocol, StudentScheduleAssociation] {

  override implicit def reads: Reads[StudentScheduleAssociationProtocol] = Json.reads[StudentScheduleAssociationProtocol]

  override implicit def writes: Writes[StudentScheduleAssociation] = Json.writes[StudentScheduleAssociation]

  override def base: String = "studentScheduleAssociations"
}
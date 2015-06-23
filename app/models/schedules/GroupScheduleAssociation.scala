package models.schedules

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}

case class GroupScheduleAssociation(date: String, timetableEntry: String, id: UUID) extends UniqueEntity

case class GroupScheduleAssociationProtocol(date: String, timetableEntry: String)

object GroupScheduleAssociation extends UriGenerator[GroupScheduleAssociation] with JsonSerialisation[GroupScheduleAssociationProtocol, GroupScheduleAssociation] {

  override implicit def reads: Reads[GroupScheduleAssociationProtocol] = Json.reads[GroupScheduleAssociationProtocol]

  override implicit def writes: Writes[GroupScheduleAssociation] = Json.writes[GroupScheduleAssociation]

  override def base: String = "groupScheduleAssociations"
}

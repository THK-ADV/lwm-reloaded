package models.schedules

import java.util.UUID
import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class GroupScheduleAssociation(date: String, timetableEntry: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object GroupScheduleAssociation extends UriGenerator[GroupScheduleAssociation] with JsonSerialisation[GroupScheduleAssociation] {
  def generateUri(groupScheduleAssociation: GroupScheduleAssociation)(implicit ns: Namespace): String = s"${ns}groupScheduleAssociations/${groupScheduleAssociation.id}"

  override implicit def reads: Reads[GroupScheduleAssociation] = Json.reads[GroupScheduleAssociation]

  override implicit def writes: Writes[GroupScheduleAssociation] = Json.writes[GroupScheduleAssociation]
}

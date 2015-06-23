package models.schedules

import java.util.UUID
import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class GroupSchedule(id: UUID = UUID.randomUUID()) extends UniqueEntity

// not included associations: []

object GroupSchedule extends UriGenerator[GroupSchedule] with JsonSerialisation[GroupSchedule]{
  def generateUri(groupSchedule: GroupSchedule)(implicit ns: Namespace): String = s"${ns}groupSchedules/${groupSchedule.id}"

  override implicit def reads: Reads[GroupSchedule] = Json.reads[GroupSchedule]

  override implicit def writes: Writes[GroupSchedule] = Json.writes[GroupSchedule]
}

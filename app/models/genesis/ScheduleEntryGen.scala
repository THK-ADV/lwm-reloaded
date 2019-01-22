package models.genesis

import java.util.UUID

import models.Group
import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import utils.Ops.JsPathX

case class ScheduleEntryGen(start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: Group)

object ScheduleEntryGen {
  import utils.LwmDateTime.{writeLocalDate, writeLocalTime, readLocalTime, readLocalDate}

  implicit val writes: Writes[ScheduleEntryGen] = (
    (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "room").write[UUID] and
      (JsPath \ "supervisor").writeSet[UUID] and
      (JsPath \ "group").write[Group](Group.writes)
    ) (unlift(ScheduleEntryGen.unapply))

  implicit val reads: Reads[ScheduleEntryGen] = Json.reads[ScheduleEntryGen]
}
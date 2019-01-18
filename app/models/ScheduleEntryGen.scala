package models

import java.util.UUID

import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import utils.Ops.JsPathX
import play.api.libs.functional.syntax._

case class ScheduleEntryGen(start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: PostgresGroup)

object ScheduleEntryGen {
  import utils.LwmDateTime.{writeLocalTime, writeLocalDate, readLocalDate, readLocalTime}

  implicit val writes: Writes[ScheduleEntryGen] = (
    (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "room").write[UUID] and
      (JsPath \ "supervisor").writeSet[UUID] and
      (JsPath \ "group").write[PostgresGroup](PostgresGroup.writes)
    ) (unlift(ScheduleEntryGen.unapply))

  implicit val reads: Reads[ScheduleEntryGen] = Json.reads[ScheduleEntryGen]
}
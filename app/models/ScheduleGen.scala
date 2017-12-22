package models

import java.util.UUID

import play.api.libs.json.{Json, Reads, Writes}

case class ScheduleGen(labwork: UUID, entries: Vector[ScheduleEntryGen])

object ScheduleGen {
  implicit val writes: Writes[ScheduleGen] = Json.writes[ScheduleGen]

  implicit val reads: Reads[ScheduleGen] = Json.reads[ScheduleGen]
}
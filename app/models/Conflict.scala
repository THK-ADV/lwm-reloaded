package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import utils.Ops.JsPathX

case class Conflict(entry: ScheduleEntryGen, members: Vector[UUID], group: PostgresGroup)

object Conflict {

  implicit val writes: Writes[Conflict] = (
    (JsPath \ "entry").write[ScheduleEntryGen] and
      (JsPath \ "members").writeSeq[UUID] and
      (JsPath \ "group").write[PostgresGroup](PostgresGroup.writes)
    ) (unlift(Conflict.unapply))
}
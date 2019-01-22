package models.genesis

import java.util.UUID

import models.Group
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import utils.Ops.JsPathX

case class Conflict(entry: ScheduleEntryGen, members: Vector[UUID], group: Group)

object Conflict {

  implicit val writes: Writes[Conflict] = (
    (JsPath \ "entry").write[ScheduleEntryGen] and
      (JsPath \ "members").writeSeq[UUID] and
      (JsPath \ "group").write[Group](Group.writes)
    ) (unlift(Conflict.unapply))
}
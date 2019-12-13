package models.assignment

import java.util.UUID

import models.{Labwork, UniqueEntity}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX

sealed trait AssignmentEntryLike extends UniqueEntity {
  def labworkId: UUID
}

case class AssignmentEntry(labwork: UUID, index: Int, label: String, types: Set[AssignmentType], duration: Int, id: UUID) extends AssignmentEntryLike {
  override def labworkId = labwork
}

case class AssignmentEntryAtom(labwork: Labwork, index: Int, label: String, types: Set[AssignmentType], duration: Int, id: UUID) extends AssignmentEntryLike {
  override def labworkId = labwork.id
}

object AssignmentEntry {
  implicit val assignmentTypeWrites: Writes[AssignmentType] =
    Writes.apply(t => JsString(AssignmentType.identifier(t)))

  implicit val writes: Writes[AssignmentEntry] = (
    (JsPath \ "labwork").write[UUID] and
      (JsPath \ "index").write[Int] and
      (JsPath \ "label").write[String] and
      (JsPath \ "types").writeSet[AssignmentType](assignmentTypeWrites) and
      (JsPath \ "duration").write[Int] and
      (JsPath \ "id").write[UUID]
    ) (unlift(AssignmentEntry.unapply))
}

object AssignmentEntryAtom {

  import AssignmentEntry.assignmentTypeWrites

  implicit val writes: Writes[AssignmentEntryAtom] = (
    (JsPath \ "labwork").write[Labwork](Labwork.writes) and
      (JsPath \ "index").write[Int] and
      (JsPath \ "label").write[String] and
      (JsPath \ "types").writeSet[AssignmentType](assignmentTypeWrites) and
      (JsPath \ "duration").write[Int] and
      (JsPath \ "id").write[UUID]
    ) (unlift(AssignmentEntryAtom.unapply))
}

object AssignmentEntryLike {

  implicit val writes: Writes[AssignmentEntryLike] = {
    case normal: AssignmentEntry => Json.toJson(normal)(AssignmentEntry.writes)
    case atom: AssignmentEntryAtom => Json.toJson(atom)(AssignmentEntryAtom.writes)
  }
}

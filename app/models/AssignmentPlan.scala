package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX

sealed trait AssignmentEntryLike extends UniqueEntity {
  def labworkId: UUID
}

case class AssignmentEntry(labwork: UUID, index: Int, label: String, types: Set[AssignmentEntryType], duration: Int, id: UUID) extends AssignmentEntryLike {
  override def labworkId = labwork
}

case class AssignmentEntryProtocol(labwork: UUID, index: Int, label: String, types: Set[AssignmentEntryType], duration: Int)

case class AssignmentEntryType(entryType: String)

case class AssignmentEntryAtom(labwork: Labwork, index: Int, label: String, types: Set[AssignmentEntryType], duration: Int, id: UUID) extends AssignmentEntryLike {
  override def labworkId = labwork.id
}

object AssignmentEntryProtocol {
  implicit val reads: Reads[AssignmentEntryProtocol] = Json.reads[AssignmentEntryProtocol]
}

object AssignmentEntry {
  implicit val writes: Writes[AssignmentEntry] = Json.writes[AssignmentEntry]
}

object AssignmentEntryAtom {
  implicit val writes: Writes[AssignmentEntryAtom] = (
    (JsPath \ "labwork").write[Labwork](Labwork.writes) and
      (JsPath \ "index").write[Int] and
      (JsPath \ "label").write[String] and
      (JsPath \ "types").writeSet[AssignmentEntryType] and
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

object AssignmentEntryType {

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)
  lazy val Attendance = AssignmentEntryType("Anwesenheitspflichtig")
  lazy val Certificate = AssignmentEntryType("Testat")
  lazy val Bonus = AssignmentEntryType("Bonus")
  lazy val Supplement = AssignmentEntryType("Zusatzleistung")

  implicit val reads: Reads[AssignmentEntryType] = Json.reads[AssignmentEntryType]

  implicit val writes: Writes[AssignmentEntryType] = Json.writes[AssignmentEntryType]
}
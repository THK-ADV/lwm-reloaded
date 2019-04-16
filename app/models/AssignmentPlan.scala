package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX

sealed trait AssignmentPlanLike extends UniqueEntity {
  def entries: Set[AssignmentEntry]

  def labworkId: UUID
}

case class AssignmentPlan(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID = UUID.randomUUID) extends AssignmentPlanLike {
  override def labworkId = labwork
}

case class AssignmentPlanProtocol(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry])

case class AssignmentEntry(index: Int, label: String, types: Set[AssignmentEntryType], duration: Int = 1)

case class AssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0)

case class AssignmentPlanAtom(labwork: Labwork, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID) extends AssignmentPlanLike {
  override def labworkId = labwork.id
}

object AssignmentPlan {
  implicit val writes: Writes[AssignmentPlan] = Json.writes[AssignmentPlan]
}

object AssignmentPlanProtocol {
  implicit val reads: Reads[AssignmentPlanProtocol] = Json.reads[AssignmentPlanProtocol]
}

object AssignmentPlanLike {

  implicit val writes: Writes[AssignmentPlanLike] = {
    case normal: AssignmentPlan => Json.toJson(normal)(AssignmentPlan.writes)
    case atom: AssignmentPlanAtom => Json.toJson(atom)(AssignmentPlanAtom.writes)
  }
}

object AssignmentPlanAtom {

  implicit val writes: Writes[AssignmentPlanAtom] = (
    (JsPath \ "labwork").write[Labwork](Labwork.writes) and
      (JsPath \ "attendance").write[Int] and
      (JsPath \ "mandatory").write[Int] and
      (JsPath \ "entries").writeSet[AssignmentEntry] and
      (JsPath \ "id").write[UUID]
    ) (unlift(AssignmentPlanAtom.unapply))
}

object AssignmentEntry {

  implicit val reads: Reads[AssignmentEntry] = Json.reads[AssignmentEntry]

  implicit val writes: Writes[AssignmentEntry] = Json.writes[AssignmentEntry]
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
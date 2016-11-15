package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX

case class AssignmentPlan(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], invalidated: Option[DateTime] = None, id: UUID = AssignmentPlan.randomUUID) extends UniqueEntity

case class AssignmentPlanProtocol(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry])

case class AssignmentEntry(index: Int, label: String, types: Set[AssignmentEntryType], duration: Int = 1)

case class AssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0)

case class AssignmentPlanAtom(labwork: Labwork, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object AssignmentPlan extends UriGenerator[AssignmentPlan] with JsonSerialisation[AssignmentPlanProtocol, AssignmentPlan, AssignmentPlanAtom] {

  lazy val empty = AssignmentPlan(UUID.randomUUID, 0, 0, Set.empty[AssignmentEntry])

  override implicit def reads: Reads[AssignmentPlanProtocol] = Json.reads[AssignmentPlanProtocol]

  override implicit def writes: Writes[AssignmentPlan] = Json.writes[AssignmentPlan]

  override implicit def writesAtom: Writes[AssignmentPlanAtom] = AssignmentPlanAtom.writesAtom;

  override def base: String = "assignmentPlans"
}

object AssignmentPlanAtom {
  implicit def writesAtom: Writes[AssignmentPlanAtom] = (
    (JsPath \ "labwork").write[Labwork] and
      (JsPath \ "attendance").write[Int] and
      (JsPath \ "mandatory").write[Int] and
      (JsPath \ "entries").writeSet[AssignmentEntry] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(AssignmentPlanAtom.unapply))
}

object AssignmentEntry extends JsonSerialisation[AssignmentEntry, AssignmentEntry, AssignmentEntry] {

  override implicit def reads: Reads[AssignmentEntry] = Json.reads[AssignmentEntry]

  override def writesAtom: Writes[AssignmentEntry] = writes

  override implicit def writes: Writes[AssignmentEntry] = Json.writes[AssignmentEntry]
}

object AssignmentEntryType extends JsonSerialisation[AssignmentEntryType, AssignmentEntryType, AssignmentEntryType] {

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)
  val Attendance = AssignmentEntryType("Anwesenheitspflichtig")
  val Certificate = AssignmentEntryType("Testat")
  val Bonus = AssignmentEntryType("Bonus")
  val Supplement = AssignmentEntryType("Zusatzleistung")

  override implicit def reads: Reads[AssignmentEntryType] = Json.reads[AssignmentEntryType]

  override def writesAtom: Writes[AssignmentEntryType] = writes

  override implicit def writes: Writes[AssignmentEntryType] = Json.writes[AssignmentEntryType]
}
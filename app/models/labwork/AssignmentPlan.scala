package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import play.api.libs.json._

case class AssignmentPlan(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID = AssignmentPlan.randomUUID) extends UniqueEntity

case class AssignmentPlanProtocol(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry])

case class AssignmentEntry(index: Int, label: String, types: Set[AssignmentEntryType], duration: Int = 1)

case class AssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0)

case class AssignmentPlanAtom(labwork: Labwork, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID)

object AssignmentPlan extends UriGenerator[AssignmentPlan] with JsonSerialisation[AssignmentPlanProtocol, AssignmentPlan] {

  lazy val empty = AssignmentPlan(UUID.randomUUID(), 0, 0, Set.empty[AssignmentEntry])

  override implicit def reads: Reads[AssignmentPlanProtocol] = Json.reads[AssignmentPlanProtocol]

  override implicit def writes: Writes[AssignmentPlan] = Json.writes[AssignmentPlan]

  override def base: String = "assignmentPlans"
}

object AssignmentEntry extends JsonSerialisation[AssignmentEntry, AssignmentEntry] {

  override implicit def reads: Reads[AssignmentEntry] = Json.reads[AssignmentEntry]

  override implicit def writes: Writes[AssignmentEntry] = Json.writes[AssignmentEntry]
}

object AssignmentEntryType extends JsonSerialisation[AssignmentEntryType, AssignmentEntryType] {

  val Attendance = AssignmentEntryType("Anwesenheitspflichtig")
  val Certificate = AssignmentEntryType("Testat")
  val Bonus = AssignmentEntryType("Bonus")
  val Supplement = AssignmentEntryType("Zusatzleistung")

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)

  override implicit def reads: Reads[AssignmentEntryType] = Json.reads[AssignmentEntryType]

  override implicit def writes: Writes[AssignmentEntryType] = Json.writes[AssignmentEntryType]
}
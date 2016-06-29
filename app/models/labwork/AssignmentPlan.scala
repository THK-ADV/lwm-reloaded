package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import play.api.libs.json._

case class AssignmentPlan(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID = AssignmentPlan.randomUUID) extends UniqueEntity

case class AssignmentPlanProtocol(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry])

case class AssignmentEntry(index: Int, label: String, types: Set[AssignmentEntryType], duration: Int = 1)

case class AssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0)

case class AssignmentPlanAtom(labwork: Labwork, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID) extends UniqueEntity

object AssignmentPlan extends UriGenerator[AssignmentPlan] with JsonSerialisation[AssignmentPlanProtocol, AssignmentPlan, AssignmentPlanAtom] {
  import AssignmentEntry._

  lazy val empty = AssignmentPlan(UUID.randomUUID(), 0, 0, Set.empty[AssignmentEntry])

  override implicit def reads: Reads[AssignmentPlanProtocol] = Json.reads[AssignmentPlanProtocol]

  override implicit def writes: Writes[AssignmentPlan] = Writes[AssignmentPlan] { item =>
      Json.obj(
        "labwork" -> item.labwork,
        "attendance" -> item.attendance,
        "entries" -> Json.toJson(item.entries),
        "mandatory" -> item.mandatory,
        "id" -> item.id
      )
  }

  override implicit def writesAtom: Writes[AssignmentPlanAtom] = Writes[AssignmentPlanAtom] { item =>
    Json.obj(
      "labwork" -> item.labwork,
      "attendance" -> item.attendance,
      "entries" -> Json.toJson(item.entries),
      "mandatory" -> item.mandatory,
      "id" -> item.id
    )
  }

  override def base: String = "assignmentPlans"
}

object AssignmentEntry extends JsonSerialisation[AssignmentEntry, AssignmentEntry, AssignmentEntry] {

  override implicit def reads: Reads[AssignmentEntry] = Json.reads[AssignmentEntry]

  override implicit def writes: Writes[AssignmentEntry] = Writes[AssignmentEntry] { entry =>
    Json.obj(
      "duration" -> entry.duration,
      "index" -> entry.index,
      "label" -> entry.label,
      "entries" -> Json.toJson(entry.types)(setWrites(AssignmentEntryType.writes))
    )
  }

  override def writesAtom: Writes[AssignmentEntry] = writes
}

object AssignmentEntryType extends JsonSerialisation[AssignmentEntryType, AssignmentEntryType, AssignmentEntryType] {

  val Attendance = AssignmentEntryType("Anwesenheitspflichtig")
  val Certificate = AssignmentEntryType("Testat")
  val Bonus = AssignmentEntryType("Bonus")
  val Supplement = AssignmentEntryType("Zusatzleistung")

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)

  override implicit def reads: Reads[AssignmentEntryType] = Json.reads[AssignmentEntryType]

  override implicit def writes: Writes[AssignmentEntryType] = Json.writes[AssignmentEntryType]

  override implicit def writesAtom: Writes[AssignmentEntryType] = writes
}
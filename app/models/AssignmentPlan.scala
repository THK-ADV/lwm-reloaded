package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Format, Json, Reads, Writes}

case class AssignmentPlan(numberOfEntries: Int, entries: Set[AssignmentEntry], id: UUID = AssignmentPlan.randomUUID) extends UniqueEntity

case class AssignmentPlanProtocol(numberOfEntries: Int, entries: Set[AssignmentEntry])

case class AssignmentEntry(index: Int, types: Set[EntryType], duration: Int = 1, id: UUID = AssignmentEntry.randomUUID) extends UniqueEntity

case class AssignmentEntryProtocol(index: Int, types: Set[EntryType], duration: Int = 1)

case class EntryType(value: String)

object EntryTypes {

  val types = Vector()
}

object AssignmentPlan extends UriGenerator[AssignmentPlan] with JsonSerialisation[AssignmentPlanProtocol, AssignmentPlan] {
  import AssignmentEntry._

  override def base: String = "assignmentPlans"

  override implicit def reads: Reads[AssignmentPlanProtocol] = Json.reads[AssignmentPlanProtocol]

  override implicit def writes: Writes[AssignmentPlan] = Json.writes[AssignmentPlan]

  implicit def format: Format[AssignmentPlan] = Json.format[AssignmentPlan]
}

object AssignmentEntry extends UriGenerator[AssignmentEntry] with JsonSerialisation[AssignmentEntryProtocol, AssignmentEntry] {
  import EntryType._

  override def base: String = "assignmentEntries"

  override implicit def reads: Reads[AssignmentEntryProtocol] = Json.reads[AssignmentEntryProtocol]

  override implicit def writes: Writes[AssignmentEntry] = Json.writes[AssignmentEntry]

  implicit def format: Format[AssignmentEntry] = Json.format[AssignmentEntry]
}

object EntryType extends JsonSerialisation[EntryType, EntryType] {

  implicit def format: Format[EntryType] = Json.format[EntryType]

  override implicit def reads: Reads[EntryType] = Json.reads[EntryType]

  override implicit def writes: Writes[EntryType] = Json.writes[EntryType]
}
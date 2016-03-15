package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json._

case class AssignmentPlan(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID = AssignmentPlan.randomUUID) extends UniqueEntity

case class AssignmentEntry(index: Int, label: String, types: Set[AssignmentEntryType], duration: Int = 1, id: UUID = AssignmentEntry.randomUUID) extends UniqueEntity

case class AssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0, id: UUID = AssignmentEntryType.randomUUID) extends UniqueEntity

case class AssignmentPlanProtocol(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[AssignmentEntryProtocol])

case class AssignmentEntryProtocol(index: Int, label: String, types: Set[AssignmentEntryTypeProtocol], duration: Int = 1)

case class AssignmentEntryTypeProtocol(entryType: String, bool: Boolean = false, int: Int = 0)

case class AssignmentPlanAtom(labwork: Labwork, attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID)

object AssignmentFormats {

  implicit def entryTypeProtocolFormat = Json.format[AssignmentEntryTypeProtocol]

  implicit def entryProtocolFormat = Json.format[AssignmentEntryProtocol]

  implicit def planProtocolFormat = Json.format[AssignmentPlanProtocol]
}

object AssignmentPlan extends UriGenerator[AssignmentPlan] with JsonSerialisation[AssignmentPlanProtocol, AssignmentPlan] {

  import AssignmentFormats._

  lazy val empty = AssignmentPlan(UUID.randomUUID(), 0, 0, Set.empty[AssignmentEntry])

  override implicit def reads: Reads[AssignmentPlanProtocol] = Json.reads[AssignmentPlanProtocol]

  override implicit def writes: Writes[AssignmentPlan] = Json.writes[AssignmentPlan]

  override def base: String = "assignmentPlans"
}

object AssignmentEntry extends UriGenerator[AssignmentEntry] with JsonSerialisation[AssignmentEntryProtocol, AssignmentEntry] {

  import AssignmentFormats._

  def toProtocol(entry: AssignmentEntry): AssignmentEntryProtocol = {
    AssignmentEntryProtocol(entry.index, entry.label, entry.types.map(AssignmentEntryType.toProtocol), entry.duration)
  }

  override implicit def reads: Reads[AssignmentEntryProtocol] = Json.reads[AssignmentEntryProtocol]

  override implicit def writes: Writes[AssignmentEntry] = Json.writes[AssignmentEntry]

  override def base: String = "assignmentEntries"
}

object AssignmentEntryType extends UriGenerator[AssignmentEntryType] with JsonSerialisation[AssignmentEntryProtocol, AssignmentEntryType] {

  import AssignmentFormats._

  val Attendance = AssignmentEntryTypeProtocol("Anwesenheitspflichtig")
  val Certificate = AssignmentEntryTypeProtocol("Testat")
  val Bonus = AssignmentEntryTypeProtocol("Bonus")
  val Supplement = AssignmentEntryTypeProtocol("Zusatzleistung")

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)

  def fromProtocol(protocol: AssignmentEntryTypeProtocol): AssignmentEntryType = {
    AssignmentEntryType(protocol.entryType, protocol.bool, protocol.int, AssignmentEntryType.randomUUID)
  }

  def toProtocol(entry: AssignmentEntryType): AssignmentEntryTypeProtocol = {
    AssignmentEntryTypeProtocol(entry.entryType, entry.bool, entry.int)
  }

  override implicit def reads: Reads[AssignmentEntryProtocol] = Json.reads[AssignmentEntryProtocol]

  override implicit def writes: Writes[AssignmentEntryType] = Json.writes[AssignmentEntryType]

  implicit def protocolWrites = Json.writes[AssignmentEntryTypeProtocol]

  implicit def entryReads = Json.reads[AssignmentEntryType]

  override def base: String = "assignmentEntryTypes"
}
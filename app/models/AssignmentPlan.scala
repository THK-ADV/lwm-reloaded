package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json._

case class AssignmentPlan(attendance: Int, mandatory: Int, entries: Set[AssignmentEntry], id: UUID = AssignmentPlan.randomUUID) extends UniqueEntity

case class AssignmentEntry(index: Int, label: String, types: Set[AssignmentEntryType], duration: Int = 1, id: UUID = AssignmentEntry.randomUUID) extends UniqueEntity

case class AssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0, id: UUID = AssignmentEntryType.randomUUID) extends UniqueEntity

sealed trait EntryType

case class Attendance(present: Boolean = false) extends EntryType

case class Certificate(done: Boolean = false) extends EntryType

case class Bonus(points: Int = 0) extends EntryType

case class Supplement(done: Boolean = false) extends EntryType

object AssignmentPlan extends UriGenerator[AssignmentPlan] with JsonSerialisation[AssignmentPlan, AssignmentPlan] {

  val empty = AssignmentPlan(0, 0, Set.empty[AssignmentEntry])

  override implicit def reads: Reads[AssignmentPlan] = Json.reads[AssignmentPlan]

  override implicit def writes: Writes[AssignmentPlan] = Json.writes[AssignmentPlan]

  implicit def format: Format[AssignmentPlan] = Json.format[AssignmentPlan]

  override def base: String = "assignmentPlans"
}

object AssignmentEntry extends UriGenerator[AssignmentEntry] with JsonSerialisation[AssignmentEntry, AssignmentEntry] {

  override implicit def reads: Reads[AssignmentEntry] = Json.reads[AssignmentEntry]

  override implicit def writes: Writes[AssignmentEntry] = Json.writes[AssignmentEntry]

  implicit def format: Format[AssignmentEntry] = Json.format[AssignmentEntry]

  override def base: String = "assignmentEntries"
}

object AssignmentEntryType extends UriGenerator[AssignmentEntryType] with JsonSerialisation[AssignmentEntryType, AssignmentEntryType] {

  lazy val Attendance = AssignmentEntryType("Anwesenheitspflichtig")
  lazy val Certificate = AssignmentEntryType("Testat")
  lazy val Bonus = AssignmentEntryType("Bonus")
  lazy val Supplement = AssignmentEntryType("Zusatzleistung")

  lazy val all = List(Attendance, Certificate, Bonus, Supplement)

  override implicit def reads: Reads[AssignmentEntryType] = Json.reads[AssignmentEntryType]

  override implicit def writes: Writes[AssignmentEntryType] = Json.writes[AssignmentEntryType]

  override def base: String = "assignmentEntryTypes"
}
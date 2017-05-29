package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX
import models.LwmDateTime._

case class SesameAssignmentPlan(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[SesameAssignmentEntry], invalidated: Option[DateTime] = None, id: UUID = SesameAssignmentPlan.randomUUID) extends UniqueEntity

case class SesameAssignmentPlanProtocol(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[SesameAssignmentEntry])

case class SesameAssignmentEntry(index: Int, label: String, types: Set[SesameAssignmentEntryType], duration: Int = 1)

case class SesameAssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0)

case class SesameAssignmentPlanAtom(labwork: SesameLabwork, attendance: Int, mandatory: Int, entries: Set[SesameAssignmentEntry], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameAssignmentPlan extends UriGenerator[SesameAssignmentPlan] with JsonSerialisation[SesameAssignmentPlanProtocol, SesameAssignmentPlan, SesameAssignmentPlanAtom] {

  lazy val empty = SesameAssignmentPlan(UUID.randomUUID, 0, 0, Set.empty[SesameAssignmentEntry])

  override implicit def reads: Reads[SesameAssignmentPlanProtocol] = Json.reads[SesameAssignmentPlanProtocol]

  override implicit def writes: Writes[SesameAssignmentPlan] = Json.writes[SesameAssignmentPlan]

  override implicit def writesAtom: Writes[SesameAssignmentPlanAtom] = SesameAssignmentPlanAtom.writesAtom

  override def base: String = "assignmentPlans"
}

object SesameAssignmentPlanAtom {
  implicit def writesAtom: Writes[SesameAssignmentPlanAtom] = (
    (JsPath \ "labwork").write[SesameLabwork] and
      (JsPath \ "attendance").write[Int] and
      (JsPath \ "mandatory").write[Int] and
      (JsPath \ "entries").writeSet[SesameAssignmentEntry] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameAssignmentPlanAtom.unapply))
}

object SesameAssignmentEntry extends JsonSerialisation[SesameAssignmentEntry, SesameAssignmentEntry, SesameAssignmentEntry] {

  override implicit def reads: Reads[SesameAssignmentEntry] = Json.reads[SesameAssignmentEntry]

  override def writesAtom: Writes[SesameAssignmentEntry] = writes

  override implicit def writes: Writes[SesameAssignmentEntry] = Json.writes[SesameAssignmentEntry]
}

object SesameAssignmentEntryType extends JsonSerialisation[SesameAssignmentEntryType, SesameAssignmentEntryType, SesameAssignmentEntryType] {

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)
  val Attendance = SesameAssignmentEntryType("Anwesenheitspflichtig")
  val Certificate = SesameAssignmentEntryType("Testat")
  val Bonus = SesameAssignmentEntryType("Bonus")
  val Supplement = SesameAssignmentEntryType("Zusatzleistung")

  override implicit def reads: Reads[SesameAssignmentEntryType] = Json.reads[SesameAssignmentEntryType]

  override def writesAtom: Writes[SesameAssignmentEntryType] = writes

  override implicit def writes: Writes[SesameAssignmentEntryType] = Json.writes[SesameAssignmentEntryType]
}

// Postgres

sealed trait AssignmentPlan extends UniqueEntity

case class PostgresAssignmentPlan(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[PostgresAssignmentEntry], id: UUID = UUID.randomUUID) extends AssignmentPlan

case class AssignmentPlanDb(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[PostgresAssignmentEntry], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueEntity {
  def toAssignmentPlan: PostgresAssignmentPlan = PostgresAssignmentPlan(labwork, attendance, mandatory, entries, id)
}

case class PostgresAssignmentPlanProtocol(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[PostgresAssignmentEntry])

case class PostgresAssignmentEntry(index: Int, label: String, types: Set[PostgresAssignmentEntryType], duration: Int = 1)

case class AssignmentEntryDb(assignmentPlan: UUID, index: Int, label: String, types: Set[AssignmentEntryTypeDb], duration: Int = 1, id: UUID = UUID.randomUUID) extends UniqueEntity

case class PostgresAssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0)

case class AssignmentEntryTypeDb(assignmentEntry: UUID, entryType: String, bool: Boolean = false, int: Int = 0, id: UUID = UUID.randomUUID) extends UniqueEntity

case class PostgresAssignmentPlanAtom(labwork: PostgresLabwork, attendance: Int, mandatory: Int, entries: Set[PostgresAssignmentEntry], id: UUID) extends AssignmentPlan

object PostgresAssignmentPlan extends JsonSerialisation[PostgresAssignmentPlanProtocol, PostgresAssignmentPlan, PostgresAssignmentPlanAtom] {

  override implicit def reads: Reads[PostgresAssignmentPlanProtocol] = Json.reads[PostgresAssignmentPlanProtocol]

  override implicit def writes: Writes[PostgresAssignmentPlan] = Json.writes[PostgresAssignmentPlan]

  override implicit def writesAtom: Writes[PostgresAssignmentPlanAtom] = PostgresAssignmentPlanAtom.writesAtom
}

object AssignmentPlanDb {
  def from(protocol: PostgresAssignmentPlanProtocol, existingId: Option[UUID]) = {
    AssignmentPlanDb(protocol.labwork, protocol.attendance, protocol.mandatory, protocol.entries, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}

object AssignmentPlan {

  implicit def writes: Writes[AssignmentPlan] = new Writes[AssignmentPlan] {
    override def writes(ap: AssignmentPlan) = ap match {
      case ap: PostgresAssignmentPlan => Json.toJson(ap)(PostgresAssignmentPlan.writes)
      case apAtom: PostgresAssignmentPlanAtom => Json.toJson(apAtom)(PostgresAssignmentPlan.writesAtom)
    }
  }
}

object PostgresAssignmentPlanAtom {

  implicit def writesAtom: Writes[PostgresAssignmentPlanAtom] = (
    (JsPath \ "labwork").write[PostgresLabwork](PostgresLabwork.writes) and
      (JsPath \ "attendance").write[Int] and
      (JsPath \ "mandatory").write[Int] and
      (JsPath \ "entries").writeSet[PostgresAssignmentEntry] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresAssignmentPlanAtom.unapply))
}

object PostgresAssignmentEntry extends JsonSerialisation[PostgresAssignmentEntry, PostgresAssignmentEntry, PostgresAssignmentEntry] {

  override implicit def reads: Reads[PostgresAssignmentEntry] = Json.reads[PostgresAssignmentEntry]

  override implicit def writes: Writes[PostgresAssignmentEntry] = Json.writes[PostgresAssignmentEntry]

  override def writesAtom: Writes[PostgresAssignmentEntry] = writes
}

object PostgresAssignmentEntryType extends JsonSerialisation[PostgresAssignmentEntryType, PostgresAssignmentEntryType, PostgresAssignmentEntryType] {

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)
  val Attendance = PostgresAssignmentEntryType("Anwesenheitspflichtig")
  val Certificate = PostgresAssignmentEntryType("Testat")
  val Bonus = PostgresAssignmentEntryType("Bonus")
  val Supplement = PostgresAssignmentEntryType("Zusatzleistung")

  override implicit def reads: Reads[PostgresAssignmentEntryType] = Json.reads[PostgresAssignmentEntryType]

  override implicit def writes: Writes[PostgresAssignmentEntryType] = Json.writes[PostgresAssignmentEntryType]

  override def writesAtom: Writes[PostgresAssignmentEntryType] = writes
}
package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime._
import utils.Ops.JsPathX

sealed trait AssignmentPlan extends UniqueEntity

case class PostgresAssignmentPlan(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[PostgresAssignmentEntry], id: UUID = UUID.randomUUID) extends AssignmentPlan

case class AssignmentPlanDb(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[PostgresAssignmentEntry], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresAssignmentPlan(labwork, attendance, mandatory, entries, id)
}

case class PostgresAssignmentPlanProtocol(labwork: UUID, attendance: Int, mandatory: Int, entries: Set[PostgresAssignmentEntry])

case class PostgresAssignmentEntry(index: Int, label: String, types: Set[PostgresAssignmentEntryType], duration: Int = 1)

case class AssignmentEntryDb(assignmentPlan: UUID, index: Int, label: String, types: Set[AssignmentEntryTypeDb], duration: Int = 1, id: UUID = UUID.randomUUID) extends UniqueEntity

case class PostgresAssignmentEntryType(entryType: String, bool: Boolean = false, int: Int = 0)

case class AssignmentEntryTypeDb(assignmentEntry: UUID, entryType: String, bool: Boolean = false, int: Int = 0, id: UUID = UUID.randomUUID) extends UniqueEntity

case class PostgresAssignmentPlanAtom(labwork: PostgresLabwork, attendance: Int, mandatory: Int, entries: Set[PostgresAssignmentEntry], id: UUID) extends AssignmentPlan

object PostgresAssignmentPlan {
  implicit val writes: Writes[PostgresAssignmentPlan] = Json.writes[PostgresAssignmentPlan]
}

object PostgresAssignmentPlanProtocol {
  implicit val reads: Reads[PostgresAssignmentPlanProtocol] = Json.reads[PostgresAssignmentPlanProtocol]
}

object AssignmentPlanDb {
  def from(protocol: PostgresAssignmentPlanProtocol, existingId: Option[UUID]) = {
    AssignmentPlanDb(protocol.labwork, protocol.attendance, protocol.mandatory, protocol.entries, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}

object AssignmentPlan {

  implicit val writes: Writes[AssignmentPlan] = new Writes[AssignmentPlan] {
    override def writes(ap: AssignmentPlan) = ap match {
      case normal: PostgresAssignmentPlan => Json.toJson(normal)(PostgresAssignmentPlan.writes)
      case atom: PostgresAssignmentPlanAtom => Json.toJson(atom)(PostgresAssignmentPlanAtom.writes)
    }
  }
}

object PostgresAssignmentPlanAtom {

  implicit val writes: Writes[PostgresAssignmentPlanAtom] = (
    (JsPath \ "labwork").write[PostgresLabwork](PostgresLabwork.writes) and
      (JsPath \ "attendance").write[Int] and
      (JsPath \ "mandatory").write[Int] and
      (JsPath \ "entries").writeSet[PostgresAssignmentEntry] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresAssignmentPlanAtom.unapply))
}

object PostgresAssignmentEntry {

  implicit val reads: Reads[PostgresAssignmentEntry] = Json.reads[PostgresAssignmentEntry]

  implicit val writes: Writes[PostgresAssignmentEntry] = Json.writes[PostgresAssignmentEntry]
}

object PostgresAssignmentEntryType {

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)
  lazy val Attendance = PostgresAssignmentEntryType("Anwesenheitspflichtig")
  lazy val Certificate = PostgresAssignmentEntryType("Testat")
  lazy val Bonus = PostgresAssignmentEntryType("Bonus")
  lazy val Supplement = PostgresAssignmentEntryType("Zusatzleistung")

  implicit val reads: Reads[PostgresAssignmentEntryType] = Json.reads[PostgresAssignmentEntryType]

  implicit val writes: Writes[PostgresAssignmentEntryType] = Json.writes[PostgresAssignmentEntryType]
}
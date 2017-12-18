package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import utils.LwmDateTime._

case class SesameReportCardEntryType(entryType: String, bool: Boolean = false, int: Int = 0, invalidated: Option[DateTime] = None, id: UUID = SesameReportCardEntryType.randomUUID) extends UniqueEntity

// POSTGRES

case class PostgresReportCardEntryType(entryType: String, bool: Option[Boolean] = None, int: Int = 0, id: UUID = UUID.randomUUID) extends UniqueEntity

case class PostgresReportCardEntryTypeProtocol(entryType: String, bool: Option[Boolean], int: Int)

// DB

case class ReportCardEntryTypeDb(reportCardEntry: Option[UUID], reportCardRetry: Option[UUID], entryType: String, bool: Option[Boolean] = None, int: Int = 0, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresReportCardEntryType(entryType, bool, int, id)
}

// COMPS

object SesameReportCardEntryType extends UriGenerator[SesameReportCardEntryType] with JsonSerialisation[SesameReportCardEntryType, SesameReportCardEntryType, SesameReportCardEntryType] {

  def all = Set(Attendance, Certificate, Bonus, Supplement)

  def Attendance = SesameReportCardEntryType(SesameAssignmentEntryType.Attendance.entryType)

  def Certificate = SesameReportCardEntryType(SesameAssignmentEntryType.Certificate.entryType)

  def Bonus = SesameReportCardEntryType(SesameAssignmentEntryType.Bonus.entryType)

  def Supplement = SesameReportCardEntryType(SesameAssignmentEntryType.Supplement.entryType)

  override def base: String = "reportCardEntryTypes"

  override implicit def reads: Reads[SesameReportCardEntryType] = Json.reads[SesameReportCardEntryType]

  override def writesAtom: Writes[SesameReportCardEntryType] = writes

  override implicit def writes: Writes[SesameReportCardEntryType] = Json.writes[SesameReportCardEntryType]
}

object PostgresReportCardEntryType extends JsonSerialisation[PostgresReportCardEntryTypeProtocol, PostgresReportCardEntryType, PostgresReportCardEntryType] {

  def all = Set(Attendance, Certificate, Bonus, Supplement)

  def Attendance = PostgresReportCardEntryType(PostgresAssignmentEntryType.Attendance.entryType)

  def Certificate = PostgresReportCardEntryType(PostgresAssignmentEntryType.Certificate.entryType)

  def Bonus = PostgresReportCardEntryType(PostgresAssignmentEntryType.Bonus.entryType)

  def Supplement = PostgresReportCardEntryType(PostgresAssignmentEntryType.Supplement.entryType)

  override implicit def reads: Reads[PostgresReportCardEntryTypeProtocol] = (
    (JsPath \ "entryType").read[String] and
      (JsPath \ "bool").readNullable[Boolean] and
      (JsPath \ "int").read[Int]
    ) (PostgresReportCardEntryTypeProtocol.apply _)

  override def writesAtom: Writes[PostgresReportCardEntryType] = writes

  override implicit def writes: Writes[PostgresReportCardEntryType] = (
    (JsPath \ "entryType").write[String] and
      (JsPath \ "bool").write[Option[Boolean]] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresReportCardEntryType.unapply))
}

object PostgresReportCardEntryTypeProtocol {

  implicit def writes: Writes[PostgresReportCardEntryTypeProtocol] = Json.writes[PostgresReportCardEntryTypeProtocol]

  implicit def reads: Reads[PostgresReportCardEntryTypeProtocol] = PostgresReportCardEntryType.reads
}
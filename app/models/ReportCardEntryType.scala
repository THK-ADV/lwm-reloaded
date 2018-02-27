package models

import java.sql.Timestamp
import java.util.UUID

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

object SesameReportCardEntryType extends UriGenerator[SesameReportCardEntryType] {

  override def base: String = "reportCardEntryTypes"

}

object PostgresReportCardEntryType {

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)

  lazy val Attendance = PostgresReportCardEntryType(PostgresAssignmentEntryType.Attendance.entryType)

  lazy val Certificate = PostgresReportCardEntryType(PostgresAssignmentEntryType.Certificate.entryType)

  lazy val Bonus = PostgresReportCardEntryType(PostgresAssignmentEntryType.Bonus.entryType)

  lazy val Supplement = PostgresReportCardEntryType(PostgresAssignmentEntryType.Supplement.entryType)

  implicit val writes: Writes[PostgresReportCardEntryType] = (
    (JsPath \ "entryType").write[String] and
      (JsPath \ "bool").write[Option[Boolean]] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresReportCardEntryType.unapply))
}

object PostgresReportCardEntryTypeProtocol {

  implicit val writes: Writes[PostgresReportCardEntryTypeProtocol] = Json.writes[PostgresReportCardEntryTypeProtocol]

  implicit val reads: Reads[PostgresReportCardEntryTypeProtocol] = (
    (JsPath \ "entryType").read[String] and
      (JsPath \ "bool").readNullable[Boolean] and
      (JsPath \ "int").read[Int]
    ) (PostgresReportCardEntryTypeProtocol.apply _)
}
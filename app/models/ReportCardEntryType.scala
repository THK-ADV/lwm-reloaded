package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

case class ReportCardEntryType(entryType: String, bool: Option[Boolean] = None, int: Int = 0, id: UUID = UUID.randomUUID) extends UniqueEntity

case class ReportCardEntryTypeProtocol(entryType: String, bool: Option[Boolean], int: Int)

object ReportCardEntryType {

  lazy val all = Set(Attendance, Certificate, Bonus, Supplement)

  lazy val Attendance = ReportCardEntryType(AssignmentEntryType.Attendance.entryType)

  lazy val Certificate = ReportCardEntryType(AssignmentEntryType.Certificate.entryType)

  lazy val Bonus = ReportCardEntryType(AssignmentEntryType.Bonus.entryType)

  lazy val Supplement = ReportCardEntryType(AssignmentEntryType.Supplement.entryType)

  lazy val BooleanBasedTypes = List(Attendance, Certificate, Supplement)

  lazy val IntBasedTypes = List(Bonus)

  implicit val writes: Writes[ReportCardEntryType] = (
    (JsPath \ "entryType").write[String] and
      (JsPath \ "bool").write[Option[Boolean]] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardEntryType.unapply))
}

object ReportCardEntryTypeProtocol {

  implicit val writes: Writes[ReportCardEntryTypeProtocol] = Json.writes[ReportCardEntryTypeProtocol]

  implicit val reads: Reads[ReportCardEntryTypeProtocol] = (
    (JsPath \ "entryType").read[String] and
      (JsPath \ "bool").readNullable[Boolean] and
      (JsPath \ "int").read[Int]
    ) (ReportCardEntryTypeProtocol.apply _)
}
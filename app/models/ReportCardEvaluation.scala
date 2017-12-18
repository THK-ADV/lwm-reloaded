package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import utils.LwmDateTime
import utils.LwmDateTime._

case class SesameReportCardEvaluation(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int, timestamp: DateTime = DateTime.now, invalidated: Option[DateTime] = None, id: UUID = SesameReportCardEvaluation.randomUUID) extends UniqueEntity

case class SesameReportCardEvaluationAtom(student: SesameStudent, labwork: SesameLabworkAtom, label: String, bool: Boolean, int: Int, timestamp: DateTime, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

// POSTGRES

sealed trait ReportCardEvaluation extends UniqueEntity {
  def lastModified: DateTime
}

case class PostgresReportCardEvaluation(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int, lastModified: DateTime, id: UUID = UUID.randomUUID) extends ReportCardEvaluation

case class PostgresReportCardEvaluationAtom(student: User, labwork: PostgresLabworkAtom, label: String, bool: Boolean, int: Int, lastModified: DateTime, id: UUID = UUID.randomUUID) extends ReportCardEvaluation

// DB

case class ReportCardEvaluationDb(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresReportCardEvaluation(student, labwork, label, bool, int, lastModified.dateTime, id)
}

// COMPS

object SesameReportCardEvaluation extends UriGenerator[SesameReportCardEvaluation] with JsonSerialisation[SesameReportCardEvaluation, SesameReportCardEvaluation, SesameReportCardEvaluationAtom] {

  override def base: String = "reportCardEvaluation"

  override implicit def reads: Reads[SesameReportCardEvaluation] = Json.reads[SesameReportCardEvaluation]

  override implicit def writes: Writes[SesameReportCardEvaluation] = (
    (JsPath \ "student").write[UUID] and
      (JsPath \ "labwork").write[UUID] and
      (JsPath \ "label").write[String] and
      (JsPath \ "bool").write[Boolean] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "timestamp").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameReportCardEvaluation.unapply))

  override implicit def writesAtom: Writes[SesameReportCardEvaluationAtom] = SesameReportCardEvaluationAtom.writesAtom
}

object SesameReportCardEvaluationAtom {

  implicit def writesAtom: Writes[SesameReportCardEvaluationAtom] = (
    (JsPath \ "student").write[SesameStudent](SesameStudent.writes) and
      (JsPath \ "labwork").write[SesameLabworkAtom] and
      (JsPath \ "label").write[String] and
      (JsPath \ "bool").write[Boolean] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "timestamp").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameReportCardEvaluationAtom.unapply))
}

object PostgresReportCardEvaluation extends JsonSerialisation[PostgresReportCardEvaluation, PostgresReportCardEvaluation, PostgresReportCardEvaluationAtom] {
  override implicit def reads: Reads[PostgresReportCardEvaluation] = Json.reads[PostgresReportCardEvaluation]

  override implicit def writes: Writes[PostgresReportCardEvaluation] = Json.writes[PostgresReportCardEvaluation]

  override implicit def writesAtom: Writes[PostgresReportCardEvaluationAtom] = PostgresReportCardEvaluationAtom.writesAtom
}

object PostgresReportCardEvaluationAtom {
  implicit def writesAtom: Writes[PostgresReportCardEvaluationAtom] = (
    (JsPath \ "student").write[User](User.writes) and
      (JsPath \ "labwork").write[PostgresLabworkAtom](PostgresLabworkAtom.writesAtom) and
      (JsPath \ "label").write[String] and
      (JsPath \ "bool").write[Boolean] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "lastModified").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresReportCardEvaluationAtom.unapply))
}

object ReportCardEvaluation {

  implicit def writes[ReportCardEvaluation]: Writes[ReportCardEvaluation] = new Writes[ReportCardEvaluation] {
    override def writes(e: ReportCardEvaluation) = e match {
      case normal: PostgresReportCardEvaluation => Json.toJson(normal)(PostgresReportCardEvaluation.writes)
      case atom: PostgresReportCardEvaluationAtom => Json.toJson(atom)(PostgresReportCardEvaluationAtom.writesAtom)
    }
  }
}
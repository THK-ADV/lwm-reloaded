package models

import java.sql.Timestamp
import java.util.UUID

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

case class PostgresReportCardEvaluationProtocol(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int)

case class PostgresReportCardEvaluationAtom(student: User, labwork: PostgresLabworkAtom, label: String, bool: Boolean, int: Int, lastModified: DateTime, id: UUID = UUID.randomUUID) extends ReportCardEvaluation

// DB

case class ReportCardEvaluationDb(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresReportCardEvaluation(student, labwork, label, bool, int, lastModified.dateTime, id)
}

// COMPS

object SesameReportCardEvaluation extends UriGenerator[SesameReportCardEvaluation] {

  override def base: String = "reportCardEvaluation"
}

object PostgresReportCardEvaluation {
  implicit val writes: Writes[PostgresReportCardEvaluation] = Json.writes[PostgresReportCardEvaluation]
}

object PostgresReportCardEvaluationProtocol {
  implicit val reads: Reads[PostgresReportCardEvaluationProtocol] = Json.reads[PostgresReportCardEvaluationProtocol]
}

object PostgresReportCardEvaluationAtom {

  implicit val writes: Writes[PostgresReportCardEvaluationAtom] = (
    (JsPath \ "student").write[User](User.writes) and
      (JsPath \ "labwork").write[PostgresLabworkAtom](PostgresLabworkAtom.writes) and
      (JsPath \ "label").write[String] and
      (JsPath \ "bool").write[Boolean] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "lastModified").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresReportCardEvaluationAtom.unapply))
}

object ReportCardEvaluation {

  implicit val writes: Writes[ReportCardEvaluation] = new Writes[ReportCardEvaluation] {
    override def writes(e: ReportCardEvaluation) = e match {
      case normal: PostgresReportCardEvaluation => Json.toJson(normal)(PostgresReportCardEvaluation.writes)
      case atom: PostgresReportCardEvaluationAtom => Json.toJson(atom)(PostgresReportCardEvaluationAtom.writes)
    }
  }
}

object ReportCardEvaluationDb {
  def from(p: PostgresReportCardEvaluationProtocol, existing: Option[UUID]) = {
    ReportCardEvaluationDb(p.student, p.labwork, p.label, p.bool, p.int, id = existing getOrElse UUID.randomUUID)
  }
}

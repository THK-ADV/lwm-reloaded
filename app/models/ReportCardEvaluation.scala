package models

import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import utils.LwmDateTime.writeDateTime

sealed trait ReportCardEvaluationLike extends UniqueEntity {
  def lastModified: DateTime
}

case class ReportCardEvaluation(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int, lastModified: DateTime, id: UUID = UUID.randomUUID) extends ReportCardEvaluationLike

case class ReportCardEvaluationProtocol(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int)

case class ReportCardEvaluationAtom(student: User, labwork: LabworkAtom, label: String, bool: Boolean, int: Int, lastModified: DateTime, id: UUID = UUID.randomUUID) extends ReportCardEvaluationLike

object ReportCardEvaluation {
  implicit val writes: Writes[ReportCardEvaluation] = Json.writes[ReportCardEvaluation]
}

object ReportCardEvaluationProtocol {
  implicit val reads: Reads[ReportCardEvaluationProtocol] = Json.reads[ReportCardEvaluationProtocol]
}

object ReportCardEvaluationAtom {

  implicit val writes: Writes[ReportCardEvaluationAtom] = (
    (JsPath \ "student").write[User](User.writes) and
      (JsPath \ "labwork").write[LabworkAtom](LabworkAtom.writes) and
      (JsPath \ "label").write[String] and
      (JsPath \ "bool").write[Boolean] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "lastModified").write[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardEvaluationAtom.unapply))
}

object ReportCardEvaluationLike {

  implicit val writes: Writes[ReportCardEvaluationLike] = {
    case normal: ReportCardEvaluation => Json.toJson(normal)(ReportCardEvaluation.writes)
    case atom: ReportCardEvaluationAtom => Json.toJson(atom)(ReportCardEvaluationAtom.writes)
  }
}



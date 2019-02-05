package models

import java.util.UUID

import models.helper.EvaluationProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class ReportCardEvaluationPattern(labwork: UUID, entryType: String, min: Int, property: EvaluationProperty, id: UUID = UUID.randomUUID) extends UniqueEntity

case class ReportCardEvaluationPatternProtocol(labwork: UUID, entryType: String, min: Int, property: EvaluationProperty)

object ReportCardEvaluationPattern {

  implicit val writes: Writes[ReportCardEvaluationPattern] = (
    (JsPath \ "labwork").write[UUID] and
      (JsPath \ "entryType").write[String] and
      (JsPath \ "min").write[Int] and
      (JsPath \ "property").write[EvaluationProperty] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardEvaluationPattern.unapply)
  )
}

object ReportCardEvaluationPatternProtocol {

  implicit val reads: Reads[ReportCardEvaluationPatternProtocol] = (
    (JsPath \ "labwork").read[UUID] and
      (JsPath \ "entryType").read[String] and
      (JsPath \ "min").read[Int] and
      (JsPath \ "property").read[EvaluationProperty]
    ) (ReportCardEvaluationPatternProtocol.apply _)
}
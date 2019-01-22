package models

import java.util.UUID

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

sealed trait EvaluationProperty {
  override def toString = this match {
    case BoolBased => "bool"
    case IntBased => "int"
  }
}

case object BoolBased extends EvaluationProperty

case object IntBased extends EvaluationProperty

object EvaluationProperty {

  def apply(string: String) = string match {
    case "bool" => BoolBased
    case "int" => IntBased
  }

  implicit val writes: Writes[EvaluationProperty] = (p: EvaluationProperty) => JsString(p.toString)

  implicit val reads: Reads[EvaluationProperty] = {
    case JsString("bool") => JsSuccess(BoolBased)
    case JsString("int") => JsSuccess(IntBased)
    case json => JsError(s"$json's value must either be 'bool' or 'int'")
  }
}
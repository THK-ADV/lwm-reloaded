package models.helper

import play.api.libs.json._

sealed trait EvaluationProperty {

  override def toString: String = this match {
    case BoolBased => "bool"
    case IntBased => "int"
  }
}

object EvaluationProperty {

  def apply(string: String): EvaluationProperty = string match {
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

case object BoolBased extends EvaluationProperty

case object IntBased extends EvaluationProperty
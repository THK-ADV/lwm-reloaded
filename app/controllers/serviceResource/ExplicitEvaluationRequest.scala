package controllers.serviceResource

import play.api.libs.functional.syntax._
import play.api.libs.json._

import java.util.UUID

sealed trait ExplicitEvaluationType

object ExplicitEvaluationType {

  case object FastForward extends ExplicitEvaluationType

  case object Fire extends ExplicitEvaluationType

  def apply(kind: String): Option[ExplicitEvaluationType] = kind match {
    case "fastForward" => Some(FastForward)
    case "fire" => Some(Fire)
    case _ => None
  }

  implicit val reads: Reads[ExplicitEvaluationType] = (json: JsValue) => {
    json
      .asOpt[String]
      .flatMap(ExplicitEvaluationType.apply)
      .fold[JsResult[ExplicitEvaluationType]](JsError(s"expected kind to be either 'fastForward 'or 'fire', but was $json"))(JsSuccess.apply(_))
  }
}

case class ExplicitEvaluationRequest(labwork: UUID, student: UUID, group: UUID, kind: ExplicitEvaluationType)

object ExplicitEvaluationRequest {
  implicit val reads: Reads[ExplicitEvaluationRequest] = (
    (JsPath \ "labwork").read[UUID] and
      (JsPath \ "student").read[UUID] and
      (JsPath \ "group").read[UUID] and
      (JsPath \ "kind").read[ExplicitEvaluationType](ExplicitEvaluationType.reads)
    ) (ExplicitEvaluationRequest.apply _)
}

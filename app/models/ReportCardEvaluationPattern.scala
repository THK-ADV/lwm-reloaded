package models

import java.sql.Timestamp
import java.util.UUID

import play.api.libs.functional.syntax._
import utils.LwmDateTime.DateTimeConverter
import org.joda.time.DateTime
import play.api.libs.json._

case class ReportCardEvaluationPattern(labwork: UUID, entryType: String, min: Int, property: EvaluationProperty, id: UUID = UUID.randomUUID) extends UniqueEntity

case class ReportCardEvaluationPatternProtocol(labwork: UUID, entryType: String, min: Int, property: EvaluationProperty)

case class ReportCardEvaluationPatternDb(labwork: UUID, entryType: String, min: Int, property: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = ReportCardEvaluationPattern(labwork, entryType, min, EvaluationProperty(property), id)
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

  implicit val writes: Writes[EvaluationProperty] = new Writes[EvaluationProperty] {
    override def writes(p: EvaluationProperty) = JsString(p.toString)
  }

  implicit val reads: Reads[EvaluationProperty] = new Reads[EvaluationProperty] {
    override def reads(json: JsValue) = json match {
      case JsString("bool") => JsSuccess(BoolBased)
      case JsString("int") => JsSuccess(IntBased)
      case _ => JsError(s"$json's value must either be 'bool' or 'int'")
    }
  }
}

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

object ReportCardEvaluationPatternDb {
  def from(protocol: ReportCardEvaluationPatternProtocol, existingId: Option[UUID]) = {
    ReportCardEvaluationPatternDb(protocol.labwork, protocol.entryType, protocol.min, protocol.property.toString, id = existingId getOrElse UUID.randomUUID)
  }
}
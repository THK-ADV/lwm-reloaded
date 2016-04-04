package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Degree(label: String, abbreviation: String, id: UUID = Degree.randomUUID) extends UniqueEntity

case class DegreeProtocol(label: String, abbreviation: String)

object Degree extends UriGenerator[Degree] with JsonSerialisation[DegreeProtocol, Degree]{

  override implicit def reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]

  override implicit def writes: Writes[Degree] = Json.writes[Degree]

  implicit def format: Format[Degree] = Json.format[Degree]

  override def base: String = "degrees"
}
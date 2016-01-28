package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}

case class Degree(label: String, description: String, id: UUID) extends UniqueEntity

case class DegreeProtocol(label: String, description: String)

object Degree extends UriGenerator[Degree] with JsonSerialisation[DegreeProtocol, Degree]{

  override implicit def reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]

  override implicit def writes: Writes[Degree] = Json.writes[Degree]

  override def base: String = "degrees"
}
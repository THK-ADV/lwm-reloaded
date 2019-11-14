package models

import java.util.UUID

import play.api.libs.json.{Json, Reads, Writes}

case class DegreeProtocol(label: String, abbreviation: String)

case class Degree(label: String, abbreviation: String, id: UUID) extends UniqueEntity

object Degree {
  implicit val writes: Writes[Degree] = Json.writes[Degree]
}

object DegreeProtocol {
  implicit val reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]
}
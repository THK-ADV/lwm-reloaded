package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}

case class Degree(label: String, abbreviation: String, invalidated: Option[DateTime] = None, id: UUID = Degree.randomUUID) extends UniqueEntity

case class DegreeProtocol(label: String, abbreviation: String)

object Degree extends UriGenerator[Degree] with JsonSerialisation[DegreeProtocol, Degree, Degree]{

  override implicit def reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]

  override implicit def writes: Writes[Degree] = Json.writes[Degree]

  override implicit def writesAtom: Writes[Degree] = writes

  override def base: String = "degrees"
}
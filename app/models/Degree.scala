package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}

case class SesameDegree(label: String, abbreviation: String, invalidated: Option[DateTime] = None, id: UUID = PostgresDegree.randomUUID) extends UniqueEntity

case class DegreeProtocol(label: String, abbreviation: String)

object PostgresDegree extends UriGenerator[SesameDegree] with JsonSerialisation[DegreeProtocol, SesameDegree, SesameDegree] {

  override implicit def reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]

  override implicit def writes: Writes[SesameDegree] = Json.writes[SesameDegree]

  override implicit def writesAtom: Writes[SesameDegree] = writes

  override def base: String = "degrees"
}

//case class Degree(label: String, abbreviation: String, invalidated: Option[DateTime] = None, id: UUID = PostgresDegree.randomUUID) extends UniqueEntity
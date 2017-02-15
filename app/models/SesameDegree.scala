package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}

case class SesameDegree(label: String, abbreviation: String, invalidated: Option[DateTime] = None, id: UUID = SesameDegree.randomUUID) extends UniqueEntity

case class DegreeProtocol(label: String, abbreviation: String)

object SesameDegree extends UriGenerator[SesameDegree] with JsonSerialisation[DegreeProtocol, SesameDegree, SesameDegree] {

  override implicit def reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]

  override implicit def writes: Writes[SesameDegree] = Json.writes[SesameDegree]

  override implicit def writesAtom: Writes[SesameDegree] = writes

  override def base: String = "degrees"
}

case class PostgresDegree(label: String, abbreviation: String, id: UUID = PostgresDegree.randomUUID) extends UniqueEntity

object PostgresDegree extends UriGenerator[PostgresDegree] with JsonSerialisation[PostgresDegree, PostgresDegree, PostgresDegree] {

  override def base = ???

  override implicit def reads: Reads[PostgresDegree] = Json.reads[PostgresDegree]

  override implicit def writes: Writes[PostgresDegree] = Json.writes[PostgresDegree]

  override implicit def writesAtom: Writes[PostgresDegree] = writes
}
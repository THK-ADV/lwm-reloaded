package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import models.LwmDateTime.DateTimeConverter
import play.api.libs.json.{Json, Reads, Writes}

case class SesameDegree(label: String, abbreviation: String, invalidated: Option[DateTime] = None, id: UUID = SesameDegree.randomUUID) extends UniqueEntity

case class DegreeProtocol(label: String, abbreviation: String)

object SesameDegree extends UriGenerator[SesameDegree] with JsonSerialisation[DegreeProtocol, SesameDegree, SesameDegree] {

  override implicit def reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]

  override implicit def writes: Writes[SesameDegree] = Json.writes[SesameDegree]

  override implicit def writesAtom: Writes[SesameDegree] = writes

  override def base: String = "degrees"
}

case class PostgresDegree(label: String, abbreviation: String, id: UUID = UUID.randomUUID) extends UniqueEntity

case class DegreeDb(label: String, abbreviation: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresDegree(label, abbreviation, id)
}

object DegreeDb {
  def from(protocol: DegreeProtocol, existingId: Option[UUID]) = {
    DegreeDb(protocol.label, protocol.abbreviation, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}

object PostgresDegree extends JsonSerialisation[DegreeProtocol, PostgresDegree, PostgresDegree] {

  override implicit def reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]

  override implicit def writes: Writes[PostgresDegree] = Json.writes[PostgresDegree]

  override implicit def writesAtom: Writes[PostgresDegree] = writes
}
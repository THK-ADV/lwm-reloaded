package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}
import utils.LwmDateTime.DateTimeConverter

case class SesameDegree(label: String, abbreviation: String, invalidated: Option[DateTime] = None, id: UUID = SesameDegree.randomUUID) extends UniqueEntity

case class DegreeProtocol(label: String, abbreviation: String)

object SesameDegree extends UriGenerator[SesameDegree] {
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

object PostgresDegree {
  implicit val writes: Writes[PostgresDegree] = Json.writes[PostgresDegree]
}

object DegreeProtocol {
  implicit val reads: Reads[DegreeProtocol] = Json.reads[DegreeProtocol]
}
package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}
import utils.LwmDateTime._

case class PostgresRoom(label: String, description: String, capacity: Int, id: UUID = UUID.randomUUID) extends UniqueEntity

case class RoomDb(label: String, description: String, capacity: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresRoom(label, description, capacity, id)
}

case class PostgresRoomProtocol(label: String, description: String, capacity: Int)

object PostgresRoom {
  implicit val writes: Writes[PostgresRoom] = Json.writes[PostgresRoom]
}

object PostgresRoomProtocol {
  implicit val reads: Reads[PostgresRoomProtocol] = Json.reads[PostgresRoomProtocol]
}

object RoomDb {
  def from(protocol: PostgresRoomProtocol, existingId: Option[UUID]): RoomDb = {
    RoomDb(protocol.label, protocol.description, protocol.capacity, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}
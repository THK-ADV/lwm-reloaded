package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}
import utils.LwmDateTime._

case class SesameRoom(label: String, description: String, invalidated: Option[DateTime] = None, id: UUID = SesameRoom.randomUUID) extends UniqueEntity

case class SesameRoomProtocol(label: String, description: String)

object SesameRoom extends UriGenerator[SesameRoom] with JsonSerialisation[SesameRoomProtocol, SesameRoom, SesameRoom] {

  lazy val default = SesameRoom("tbd", "tbd")

  override implicit def reads: Reads[SesameRoomProtocol] = Json.reads[SesameRoomProtocol]

  override implicit def writes: Writes[SesameRoom] = Json.writes[SesameRoom]

  override implicit def writesAtom: Writes[SesameRoom] = writes

  override def base: String = "rooms"
}

case class PostgresRoom(label: String, description: String, capacity: Int, id: UUID = PostgresRoom.randomUUID) extends UniqueEntity

case class RoomDb(label: String, description: String, capacity: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = PostgresRoom.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresRoom(label, description, capacity, id)
}

case class PostgresRoomProtocol(label: String, description: String, capacity: Int)

object PostgresRoom extends UriGenerator[PostgresRoom] with JsonSerialisation[PostgresRoomProtocol, PostgresRoom, PostgresRoom] {

  lazy val default = PostgresRoom("tbd", "tbd", 0)

  override implicit def reads: Reads[PostgresRoomProtocol] = Json.reads[PostgresRoomProtocol]

  override implicit def writes: Writes[PostgresRoom] = Json.writes[PostgresRoom]

  override implicit def writesAtom: Writes[PostgresRoom] = writes

  override def base: String = "rooms"
}

object RoomDb{
  def from(protocol: PostgresRoomProtocol, existingId: Option[UUID]) : RoomDb = {
    RoomDb(protocol.label, protocol.description, protocol.capacity, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}
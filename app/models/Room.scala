package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}

case class Room(label: String, description: String, id: UUID = Room.randomUUID) extends UniqueEntity

case class RoomProtocol(label: String, description: String)

object Room extends UriGenerator[Room] with JsonSerialisation[RoomProtocol, Room] {

  override implicit def reads: Reads[RoomProtocol] = Json.reads[RoomProtocol]

  override implicit def writes: Writes[Room] = Json.writes[Room]

  override def base: String = "rooms"
}
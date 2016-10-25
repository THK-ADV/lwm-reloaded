package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Room(label: String, description: String, invalidated: Option[DateTime] = None, id: UUID = Room.randomUUID) extends UniqueEntity

case class RoomProtocol(label: String, description: String)

object Room extends UriGenerator[Room] with JsonSerialisation[RoomProtocol, Room, Room] {

  lazy val default = Room("tbd", "tbd")

  override implicit def reads: Reads[RoomProtocol] = Json.reads[RoomProtocol]

  override implicit def writes: Writes[Room] = Json.writes[Room]

  override implicit def writesAtom: Writes[Room] = writes

  //implicit def format: Format[Room] = Json.format[Room]

  override def base: String = "rooms"
}
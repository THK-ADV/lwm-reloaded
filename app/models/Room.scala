package models

import java.util.UUID

import play.api.libs.json.{Json, Reads, Writes}

case class Room(label: String, description: String, capacity: Int, id: UUID) extends UniqueEntity

case class RoomProtocol(label: String, description: String, capacity: Int)

object Room {
  implicit val writes: Writes[Room] = Json.writes[Room]
}

object RoomProtocol {
  implicit val reads: Reads[RoomProtocol] = Json.reads[RoomProtocol]
}


package models

import java.util.UUID
import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Room(label: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Room extends UriGenerator[Room] with JsonSerialisation[Room] {
  def generateUri(room: Room)(implicit ns: Namespace): String = s"${ns}rooms/${room.id}"

  override implicit def reads: Reads[Room] = Json.reads[Room]

  override implicit def writes: Writes[Room] = Json.writes[Room]
}
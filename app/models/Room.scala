package models

import java.util.UUID
import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Room(label: String, id: Option[UUID] = Some(UUID.randomUUID())) extends UniqueEntity

object Room extends UriGenerator[Room] with JsonSerialisation[Room] {

  override implicit def reads: Reads[Room] = Json.reads[Room]

  override implicit def writes: Writes[Room] = Json.writes[Room]

  override def base: String = "rooms"
}
package models

import java.util.UUID

import store.Namespace

case class Room(label: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Room extends UriGenerator[Room] {
  def generateUri(room: Room)(implicit ns: Namespace): String = s"${ns}rooms/${room.id}"
}
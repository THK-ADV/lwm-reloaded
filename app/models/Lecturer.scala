package models

import java.util.UUID

import play.api.libs.json.{Json, Reads, Writes}

case class Lecturer(systemId: String, lastname: String, firstname: String, email: String, id: UUID = UUID.randomUUID) extends User

object Lecturer {
  implicit val writes: Writes[Lecturer] = Json.writes[Lecturer]
}
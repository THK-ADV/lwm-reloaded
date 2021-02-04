package models

import play.api.libs.json.{Json, Reads, Writes}

import java.util.UUID

case class Role(label: String, id: UUID = UUID.randomUUID) extends UniqueEntity

object Role {
  implicit val writes: Writes[Role] = Json.writes[Role]
  implicit val reads: Reads[Role] = Json.reads[Role]
}
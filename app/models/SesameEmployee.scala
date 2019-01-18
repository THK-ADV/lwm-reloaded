package models

import java.util.UUID

import play.api.libs.json.{Json, Writes}

case class PostgresEmployee(systemId: String, lastname: String, firstname: String, email: String, id: UUID = UUID.randomUUID) extends User

object PostgresEmployee {
  implicit val writes: Writes[PostgresEmployee] = Json.writes[PostgresEmployee]
}
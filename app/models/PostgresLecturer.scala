package models

import java.util.UUID

import play.api.libs.json.{Json, Reads, Writes}

case class PostgresLecturer(systemId: String, lastname: String, firstname: String, email: String, id: UUID = User.randomUUID) extends User

object PostgresLecturer {
  implicit val writes: Writes[PostgresLecturer] = Json.writes[PostgresLecturer]
}
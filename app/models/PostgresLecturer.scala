package models

import java.util.UUID

import controllers.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}

case class PostgresLecturer(systemId: String, lastname: String, firstname: String, email: String, id: UUID = User.randomUUID) extends User

object PostgresLecturer extends JsonSerialisation[PostgresLecturer, PostgresLecturer, PostgresLecturer] {

  override implicit def reads: Reads[PostgresLecturer] = Json.reads[PostgresLecturer]

  override implicit def writes: Writes[PostgresLecturer] = Json.writes[PostgresLecturer]

  override implicit def writesAtom: Writes[PostgresLecturer] = ???
}
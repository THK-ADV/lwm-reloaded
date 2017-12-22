package models

import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.json.{Json, Writes}

case class SesameEmployee(systemId: String, lastname: String, firstname: String, email: String, status: String, invalidated: Option[DateTime] = None, id: UUID = User.randomUUID) extends User

case class PostgresEmployee(systemId: String, lastname: String, firstname: String, email: String, id: UUID = User.randomUUID) extends User

object PostgresEmployee {
  implicit val writes: Writes[PostgresEmployee] = Json.writes[PostgresEmployee]
}
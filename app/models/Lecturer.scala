package models

import database.helper.LdapUserStatus
import play.api.libs.json.{Json, Writes}

import java.util.UUID

case class Lecturer(
    systemId: String,
    campusId: String,
    lastname: String,
    firstname: String,
    email: String,
    id: UUID = UUID.randomUUID
) extends User {
  override def status = LdapUserStatus.LecturerStatus
}

object Lecturer {
  implicit val writes: Writes[Lecturer] = Json.writes[Lecturer]
}

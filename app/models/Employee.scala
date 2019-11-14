package models

import java.util.UUID

import play.api.libs.json.{Json, Writes}

case class Employee(systemId: String, lastname: String, firstname: String, email: String, id: UUID) extends User

object Employee {
  implicit val writes: Writes[Employee] = Json.writes[Employee]
}
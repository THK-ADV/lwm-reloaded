package models

import database.helper.LdapUserStatus
import play.api.libs.json.{Json, Writes}

import java.util.UUID

case class Employee(systemId: String, lastname: String, firstname: String, email: String, id: UUID) extends User {
  override def status = LdapUserStatus.EmployeeStatus
}

object Employee {
  implicit val writes: Writes[Employee] = Json.writes[Employee]
}
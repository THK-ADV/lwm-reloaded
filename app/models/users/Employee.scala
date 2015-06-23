package models.users

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}

case class Employee(systemId: String, lastname: String, firstname: String, email: String, id: UUID) extends User

case class EmployeeProtocol(systemId: String, lastname: String, firstname: String, email: String)

object Employee extends UriGenerator[Employee] with JsonSerialisation[EmployeeProtocol, Employee] {

  override implicit def reads: Reads[EmployeeProtocol] = Json.reads[EmployeeProtocol]

  override implicit def writes: Writes[Employee] = Json.writes[Employee]

  override def base: String = "employees"
}

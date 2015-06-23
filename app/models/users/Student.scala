package models.users

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}

case class Student(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, id: UUID) extends User

case class StudentProtocol(systemId: String, lastname: String, firstname: String, email: String, registrationId: String)

object Student extends UriGenerator[Student] with JsonSerialisation[StudentProtocol, Student] {

  override implicit def reads: Reads[StudentProtocol] = Json.reads[StudentProtocol]

  override implicit def writes: Writes[Student] = Json.writes[Student]

  override def base: String = "students"
}

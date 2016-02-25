package models.users

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}

case class Student(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, id: UUID) extends User

case class StudentProtocol(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID)

case class StudentAtom(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: Degree, id: UUID)

object Student extends UriGenerator[Student] with JsonSerialisation[StudentProtocol, Student] {

  override implicit def reads: Reads[StudentProtocol] = Json.reads[StudentProtocol]

  override implicit def writes: Writes[Student] = Json.writes[Student]

  implicit def atomicWrites: Writes[StudentAtom] = Json.writes[StudentAtom]

  override def base: String = "students"
}

package models.users

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}

case class Student(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, id: UUID = User.randomUUID) extends User

case class StudentAtom(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: Degree, id: UUID) extends UniqueEntity

object Student extends JsonSerialisation[Student, Student] {

  override implicit def reads: Reads[Student] = Json.reads[Student]

  override implicit def writes: Writes[Student] = Json.writes[Student]

  implicit def atomicWrites: Writes[StudentAtom] = Json.writes[StudentAtom]
}

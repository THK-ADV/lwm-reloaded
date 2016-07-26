package models.users

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}

case class Student(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, invalidated: Option[DateTime] = None, id: UUID = User.randomUUID) extends User

case class StudentAtom(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: Degree, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Student extends JsonSerialisation[Student, Student, StudentAtom] {

  override implicit def reads: Reads[Student] = Json.reads[Student]

  override implicit def writes: Writes[Student] = Json.writes[Student]

  override implicit def writesAtom: Writes[StudentAtom] = Json.writes[StudentAtom]
}

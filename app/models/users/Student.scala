package models.users

import java.util.UUID

import controllers.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Student(systemId: String,
                   lastname: String,
                   firstname: String,
                   email: String,
                   registrationId: String, id: UUID = UUID.randomUUID()) extends User

object Student extends UriGenerator[Student] with JsonSerialisation[Student] {
  def generateUri(student: Student)(implicit ns: Namespace): String = s"${ns}students/${student.id}"

  override implicit def reads: Reads[Student] = Json.reads[Student]

  override implicit def writes: Writes[Student] = Json.writes[Student]
}

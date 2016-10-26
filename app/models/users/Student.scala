package models.users

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import org.joda.time.DateTime
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX

case class Student(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, invalidated: Option[DateTime] = None, id: UUID = User.randomUUID) extends User

case class StudentAtom(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: Degree, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Student extends JsonSerialisation[Student, Student, StudentAtom] {

  override implicit def reads: Reads[Student] = Json.reads[Student]

  override implicit def writes: Writes[Student] = Json.writes[Student]

  override implicit def writesAtom: Writes[StudentAtom] = StudentAtom.writesAtom
}
object StudentAtom{
  implicit def writesAtom: Writes[StudentAtom] = (
    (JsPath \ "systemId").write[String] and
      (JsPath \ "lastname").write[String] and
      (JsPath \ "firstname").write[String] and
      (JsPath \ "email").write[String] and
      (JsPath \ "registrationId").write[String] and
      (JsPath \ "enrollment").write[Degree](Degree.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    )(unlift(StudentAtom.unapply))
}

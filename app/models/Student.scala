package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Writes}

case class Student(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, id: UUID = UUID.randomUUID) extends User

case class StudentAtom(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: Degree, id: UUID) extends User

object Student {
  implicit val writes: Writes[Student] = Json.writes[Student]
}

object StudentAtom {

  implicit val writes: Writes[StudentAtom] = (
    (JsPath \ "systemId").write[String] and
      (JsPath \ "lastname").write[String] and
      (JsPath \ "firstname").write[String] and
      (JsPath \ "email").write[String] and
      (JsPath \ "registrationId").write[String] and
      (JsPath \ "enrollment").write[Degree](Degree.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(StudentAtom.unapply))
}
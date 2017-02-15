package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

case class SesameStudent(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, invalidated: Option[DateTime] = None, id: UUID = User.randomUUID) extends User
case class SesameStudentAtom(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: Degree, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameStudent extends JsonSerialisation[SesameStudent, SesameStudent, SesameStudentAtom] {

  override implicit def reads: Reads[SesameStudent] = Json.reads[SesameStudent]

  override implicit def writes: Writes[SesameStudent] = Json.writes[SesameStudent]

  override implicit def writesAtom: Writes[SesameStudentAtom] = SesameStudentAtom.writesAtom
}

object SesameStudentAtom {

  implicit def writesAtom: Writes[SesameStudentAtom] = (
    (JsPath \ "systemId").write[String] and
      (JsPath \ "lastname").write[String] and
      (JsPath \ "firstname").write[String] and
      (JsPath \ "email").write[String] and
      (JsPath \ "registrationId").write[String] and
      (JsPath \ "enrollment").write[Degree](Degree.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameStudentAtom.unapply))
}

case class PostgresStudent(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, id: UUID = User.randomUUID) extends User

object PostgresStudent extends JsonSerialisation[PostgresStudent, PostgresStudent, PostgresStudent] {

  override implicit def reads: Reads[PostgresStudent] = Json.reads[PostgresStudent]

  override implicit def writes: Writes[PostgresStudent] = Json.writes[PostgresStudent]

  override implicit def writesAtom: Writes[PostgresStudent] = ???
}
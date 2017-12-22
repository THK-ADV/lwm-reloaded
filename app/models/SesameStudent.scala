package models

import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

case class SesameStudent(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, invalidated: Option[DateTime] = None, id: UUID = User.randomUUID) extends User

case class SesameStudentAtom(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: SesameDegree, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class PostgresStudent(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID, id: UUID = User.randomUUID) extends User

case class PostgresStudentAtom(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: PostgresDegree, id: UUID) extends User

object PostgresStudent {
  implicit val writes: Writes[PostgresStudent] = Json.writes[PostgresStudent]
}

object PostgresStudentAtom {

  implicit val writes: Writes[PostgresStudentAtom] = (
    (JsPath \ "systemId").write[String] and
      (JsPath \ "lastname").write[String] and
      (JsPath \ "firstname").write[String] and
      (JsPath \ "email").write[String] and
      (JsPath \ "registrationId").write[String] and
      (JsPath \ "enrollment").write[PostgresDegree](PostgresDegree.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresStudentAtom.unapply))
}
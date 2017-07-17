package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import models.LwmDateTime.DateTimeConverter

case class SesameLabwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, invalidated: Option[DateTime] = None, id: UUID = SesameLabwork.randomUUID) extends UniqueEntity

case class LabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean, published: Boolean)

case class SesameLabworkAtom(label: String, description: String, semester: SesameSemester, course: SesameCourseAtom, degree: SesameDegree, subscribable: Boolean, published: Boolean, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameLabwork extends UriGenerator[SesameLabwork] with JsonSerialisation[LabworkProtocol, SesameLabwork, SesameLabworkAtom] {

  override def base: String = "labworks"

  override implicit def reads: Reads[LabworkProtocol] = Json.reads[LabworkProtocol]

  override implicit def writes: Writes[SesameLabwork] = Json.writes[SesameLabwork]

  override implicit def writesAtom: Writes[SesameLabworkAtom] = SesameLabworkAtom.writesAtom
}

object SesameLabworkAtom {

  implicit def writesAtom: Writes[SesameLabworkAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "semester").write[SesameSemester] and
      (JsPath \ "course").write[SesameCourseAtom] and
      (JsPath \ "degree").write[SesameDegree](SesameDegree.writes) and
      (JsPath \ "subscribable").write[Boolean] and
      (JsPath \ "published").write[Boolean] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameLabworkAtom.unapply))
}

// Postgres

sealed trait Labwork extends UniqueEntity

case class PostgresLabwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, id: UUID = UUID.randomUUID) extends Labwork

case class PostgresLabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean, published: Boolean)

case class PostgresLabworkAtom(label: String, description: String, semester: PostgresSemester, course: PostgresCourseAtom, degree: PostgresDegree, subscribable: Boolean, published: Boolean, id: UUID) extends Labwork

case class LabworkDb(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresLabwork(label, description, semester, course, degree, subscribable, published, id)
}

object PostgresLabwork extends JsonSerialisation[PostgresLabworkProtocol, PostgresLabwork, PostgresLabworkAtom] {
  override implicit def reads: Reads[PostgresLabworkProtocol] = Json.reads[PostgresLabworkProtocol]

  override implicit def writes: Writes[PostgresLabwork] = Json.writes[PostgresLabwork]

  override implicit def writesAtom: Writes[PostgresLabworkAtom] = PostgresLabworkAtom.writesAtom
}

object Labwork{
  implicit def writes: Writes[Labwork] = new Writes[Labwork] {
    override def writes(labwork: Labwork): JsValue = labwork match {
      case postgresLabwork: PostgresLabwork => Json.toJson(postgresLabwork)(PostgresLabwork.writes)
      case postgresLabworkAtom: PostgresLabworkAtom => Json.toJson(postgresLabworkAtom)(PostgresLabwork.writesAtom)
    }
  }
  implicit def reads: Reads[PostgresLabworkProtocol] = Json.reads[PostgresLabworkProtocol]
}

object PostgresLabworkAtom {

  implicit def writesAtom: Writes[PostgresLabworkAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "semester").write[PostgresSemester](PostgresSemester.writes) and
      (JsPath \ "course").write[PostgresCourseAtom](PostgresCourseAtom.writesAtom) and
      (JsPath \ "degree").write[PostgresDegree](PostgresDegree.writes) and
      (JsPath \ "subscribable").write[Boolean] and
      (JsPath \ "published").write[Boolean] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresLabworkAtom.unapply))
}

object LabworkDb{
  def from(protocol: PostgresLabworkProtocol, existingId: Option[UUID]): LabworkDb = {
    LabworkDb(protocol.label, protocol.description, protocol.semester, protocol.course, protocol.degree, protocol.subscribable, protocol.published, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID()))
  }
}
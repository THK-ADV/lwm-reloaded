package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime.DateTimeConverter

case class SesameLabwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, invalidated: Option[DateTime] = None, id: UUID = SesameLabwork.randomUUID) extends UniqueEntity

case class LabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean, published: Boolean)

case class SesameLabworkAtom(label: String, description: String, semester: SesameSemester, course: SesameCourseAtom, degree: SesameDegree, subscribable: Boolean, published: Boolean, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameLabwork extends UriGenerator[SesameLabwork] {
  override def base: String = "labworks"
}

// Postgres

sealed trait Labwork extends UniqueEntity

case class PostgresLabwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, id: UUID = UUID.randomUUID) extends Labwork

case class PostgresLabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean, published: Boolean)

case class PostgresLabworkAtom(label: String, description: String, semester: PostgresSemester, course: PostgresCourseAtom, degree: PostgresDegree, subscribable: Boolean, published: Boolean, id: UUID) extends Labwork

case class LabworkDb(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresLabwork(label, description, semester, course, degree, subscribable, published, id)
}

object PostgresLabwork {
  implicit val writes: Writes[PostgresLabwork] = Json.writes[PostgresLabwork]
}

object PostgresLabworkProtocol {
  implicit val reads: Reads[PostgresLabworkProtocol] = Json.reads[PostgresLabworkProtocol]
}

object Labwork {

  implicit val writes: Writes[Labwork] = new Writes[Labwork] {
    override def writes(labwork: Labwork): JsValue = labwork match {
      case normal: PostgresLabwork => Json.toJson(normal)(PostgresLabwork.writes)
      case atom: PostgresLabworkAtom => Json.toJson(atom)(PostgresLabworkAtom.writes)
    }
  }
}

object PostgresLabworkAtom {

  implicit val writes: Writes[PostgresLabworkAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "semester").write[PostgresSemester](PostgresSemester.writes) and
      (JsPath \ "course").write[PostgresCourseAtom](PostgresCourseAtom.writes) and
      (JsPath \ "degree").write[PostgresDegree](PostgresDegree.writes) and
      (JsPath \ "subscribable").write[Boolean] and
      (JsPath \ "published").write[Boolean] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresLabworkAtom.unapply))
}

object LabworkDb {
  def from(protocol: PostgresLabworkProtocol, existingId: Option[UUID]): LabworkDb = {
    LabworkDb(protocol.label, protocol.description, protocol.semester, protocol.course, protocol.degree, protocol.subscribable, protocol.published, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID()))
  }
}
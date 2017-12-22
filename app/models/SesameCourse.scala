package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime.DateTimeConverter

case class SesameCourse(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, invalidated: Option[DateTime] = None, id: UUID = SesameCourse.randomUUID) extends UniqueEntity

case class SesameCourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class SesameCourseAtom(label: String, description: String, abbreviation: String, lecturer: SesameEmployee, semesterIndex: Int, invalidated: Option[DateTime], id: UUID) extends UniqueEntity

object SesameCourse extends UriGenerator[SesameCourse] {
  override def base: String = "courses"
}

// Postgres

sealed trait Course extends UniqueEntity

object Course {

  implicit val writes: Writes[Course] = new Writes[Course] {
    override def writes(course: Course) = course match {
      case normal: PostgresCourse => Json.toJson(normal)(PostgresCourse.writes)
      case atom: PostgresCourseAtom => Json.toJson(atom)(PostgresCourseAtom.writes)
    }
  }
}

case class PostgresCourse(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, id: UUID = UUID.randomUUID) extends Course

case class PostgresCourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class PostgresCourseAtom(label: String, description: String, abbreviation: String, lecturer: User, semesterIndex: Int, id: UUID) extends Course

case class CourseDb(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresCourse(label, description, abbreviation, lecturer, semesterIndex, id)
}

object PostgresCourse {
  implicit val writes: Writes[PostgresCourse] = Json.writes[PostgresCourse]
}

object PostgresCourseProtocol {
  implicit val reads: Reads[PostgresCourseProtocol] = Json.reads[PostgresCourseProtocol]
}

object PostgresCourseAtom {

  implicit val writes: Writes[PostgresCourseAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "abbreviation").write[String] and
      (JsPath \ "lecturer").write[User](User.writes) and
      (JsPath \ "semesterIndex").write[Int] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresCourseAtom.unapply))
}

object CourseDb {
  def from(protocol: PostgresCourseProtocol, existingId: Option[UUID]) = {
    CourseDb(protocol.label, protocol.description, protocol.abbreviation, protocol.lecturer, protocol.semesterIndex, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}
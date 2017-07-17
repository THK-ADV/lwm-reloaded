package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import models.LwmDateTime.DateTimeConverter

case class SesameCourse(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, invalidated: Option[DateTime] = None, id: UUID = SesameCourse.randomUUID) extends UniqueEntity

case class SesameCourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class SesameCourseAtom(label: String, description: String, abbreviation: String, lecturer: SesameEmployee, semesterIndex: Int, invalidated: Option[DateTime], id: UUID) extends UniqueEntity

object SesameCourse extends UriGenerator[SesameCourse] with JsonSerialisation[SesameCourseProtocol, SesameCourse, SesameCourseAtom] {

  override implicit def reads: Reads[SesameCourseProtocol] = Json.reads[SesameCourseProtocol]

  override implicit def writes: Writes[SesameCourse] = Json.writes[SesameCourse]

  override implicit def writesAtom: Writes[SesameCourseAtom] = SesameCourseAtom.writesAtom

  override def base: String = "courses"
}

object SesameCourseAtom {
  implicit def writesAtom: Writes[SesameCourseAtom] = (
      (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "abbreviation").write[String] and
      (JsPath \ "lecturer").write[SesameEmployee](SesameEmployee.writes) and
      (JsPath \ "semesterIndex").write[Int] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    )(unlift(SesameCourseAtom.unapply))
}


// Postgres

sealed trait Course extends UniqueEntity

object Course {
  implicit def writes: Writes[Course] = new Writes[Course] {
    override def writes(course: Course) = course match {
      case postgresCourse: PostgresCourse => Json.toJson(postgresCourse)(PostgresCourse.writes)
      case postgresCourseAtom: PostgresCourseAtom => Json.toJson(postgresCourseAtom)(PostgresCourseAtom.writesAtom)
    }
  }
}

case class PostgresCourse(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, id: UUID = PostgresCourse.randomUUID) extends Course

case class PostgresCourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class PostgresCourseAtom(label: String, description: String, abbreviation: String, lecturer: User, semesterIndex: Int, id: UUID) extends Course

case class CourseDb(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = PostgresCourse.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresCourse(label, description, abbreviation, lecturer, semesterIndex, id)
}

object PostgresCourse extends UriGenerator[PostgresCourse] with JsonSerialisation[PostgresCourseProtocol, PostgresCourse, PostgresCourseAtom] {

  override implicit def reads: Reads[PostgresCourseProtocol] = Json.reads[PostgresCourseProtocol]

  override implicit def writes: Writes[PostgresCourse] = Json.writes[PostgresCourse]

  override implicit def writesAtom: Writes[PostgresCourseAtom] = PostgresCourseAtom.writesAtom

  override def base: String = "courses"
}

object PostgresCourseAtom {
  implicit def writesAtom: Writes[PostgresCourseAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "abbreviation").write[String] and
      (JsPath \ "lecturer").write[User](User.writes) and
      (JsPath \ "semesterIndex").write[Int] and
      (JsPath \ "id").write[UUID]
    )(unlift(PostgresCourseAtom.unapply))
}

object CourseDb{
  def from(protocol: PostgresCourseProtocol, existingId: Option[UUID]) = {
    CourseDb(protocol.label, protocol.description, protocol.abbreviation, protocol.lecturer, protocol.semesterIndex, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}
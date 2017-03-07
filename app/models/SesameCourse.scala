package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class SesameCourse(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, invalidated: Option[DateTime] = None, id: UUID = SesameCourse.randomUUID) extends UniqueEntity

case class SesameCourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class SesameCourseAtom(label: String, description: String, abbreviation: String, lecturer: SesameEmployee, semesterIndex: Int, invalidated: Option[DateTime], id: UUID) extends UniqueEntity

case class PostgresCourse(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, id: UUID = PostgresCourse.randomUUID) extends UniqueEntity

case class PostgresCourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class PostgresCourseAtom(label: String, description: String, abbreviation: String, lecturer: PostgresEmployee, semesterIndex: Int, id: UUID) extends UniqueEntity

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
      (JsPath \ "lecturer").write[PostgresEmployee](PostgresEmployee.writes) and
      (JsPath \ "semesterIndex").write[Int] and
      (JsPath \ "id").write[UUID]
    )(unlift(PostgresCourseAtom.unapply))
}
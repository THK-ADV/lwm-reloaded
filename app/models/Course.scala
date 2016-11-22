package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Course(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, invalidated: Option[DateTime] = None, id: UUID = Course.randomUUID) extends UniqueEntity

case class CourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class CourseAtom(label: String, description: String, abbreviation: String, lecturer: Employee, semesterIndex: Int, invalidated: Option[DateTime], id: UUID) extends UniqueEntity

object Course extends UriGenerator[Course] with JsonSerialisation[CourseProtocol, Course, CourseAtom] {

  override implicit def reads: Reads[CourseProtocol] = Json.reads[CourseProtocol]

  override implicit def writes: Writes[Course] = Json.writes[Course]

  override implicit def writesAtom: Writes[CourseAtom] = CourseAtom.writesAtom

  override def base: String = "courses"
}

object CourseAtom {
  implicit def writesAtom: Writes[CourseAtom] = (
      (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "abbreviation").write[String] and
      (JsPath \ "lecturer").write[Employee](Employee.writes) and
      (JsPath \ "semesterIndex").write[Int] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    )(unlift(CourseAtom.unapply))
}
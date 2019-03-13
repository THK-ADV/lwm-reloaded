package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json._

sealed trait CourseLike extends UniqueEntity

case class Course(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, id: UUID = UUID.randomUUID) extends CourseLike

case class CourseAtom(label: String, description: String, abbreviation: String, lecturer: User, semesterIndex: Int, id: UUID) extends CourseLike

case class CourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

object CourseLike {

  implicit val writes: Writes[CourseLike] = {
    case normal: Course => Json.toJson(normal)(Course.writes)
    case atom: CourseAtom => Json.toJson(atom)(CourseAtom.writes)
  }
}

object Course {
  implicit val writes: Writes[Course] = Json.writes[Course]
}

object CourseProtocol {
  implicit val reads: Reads[CourseProtocol] = Json.reads[CourseProtocol]
}

object CourseAtom {

  implicit val writes: Writes[CourseAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "abbreviation").write[String] and
      (JsPath \ "lecturer").write[User](User.writes) and
      (JsPath \ "semesterIndex").write[Int] and
      (JsPath \ "id").write[UUID]
    ) (unlift(CourseAtom.unapply))
}


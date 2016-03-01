package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Employee
import play.api.libs.json.{Json, Reads, Writes}

case class Course(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, id: UUID) extends UniqueEntity

case class CourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class CourseAtom(label: String, description: String, abbreviation: String, lecturer: Employee, semesterIndex: Int, id: UUID)

object Course extends UriGenerator[Course] with JsonSerialisation[CourseProtocol, Course] {
  import models.users.Employee._

  override implicit def reads: Reads[CourseProtocol] = Json.reads[CourseProtocol]

  override implicit def writes: Writes[Course] = Json.writes[Course]

  implicit def atomicWrites: Writes[CourseAtom] = Json.writes[CourseAtom]

  override def base: String = "courses"
}
package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Employee
import org.joda.time.DateTime
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Course(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, invalidated: Option[DateTime] = None, id: UUID = Course.randomUUID) extends UniqueEntity

case class CourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

case class CourseAtom(label: String, description: String, abbreviation: String, lecturer: Employee, semesterIndex: Int, invalidated: Option[DateTime], id: UUID) extends UniqueEntity

object Course extends UriGenerator[Course] with JsonSerialisation[CourseProtocol, Course, CourseAtom] {
  import models.users.Employee._

  override implicit def reads: Reads[CourseProtocol] = Json.reads[CourseProtocol]

  override implicit def writes: Writes[Course] = Json.writes[Course]

  override implicit def writesAtom: Writes[CourseAtom] = Json.writes[CourseAtom]

  implicit def atomicFormat: Format[CourseAtom] = Json.format[CourseAtom]

  override def base: String = "courses"
}
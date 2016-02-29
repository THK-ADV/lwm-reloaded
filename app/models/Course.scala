package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.User
import play.api.libs.json.{Json, Reads, Writes}

case class Course(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, id: UUID) extends UniqueEntity

case class CourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int)

object Course extends UriGenerator[Course] with JsonSerialisation[CourseProtocol, Course] {
  import models.users.Employee._

  override implicit def reads: Reads[CourseProtocol] = Json.reads[CourseProtocol]

  override implicit def writes: Writes[Course] = Json.writes[Course]

  override def base: String = "courses"
}
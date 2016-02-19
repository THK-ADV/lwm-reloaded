package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}

case class Course(label: String, description: String, abbreviation: String, lecturer: UUID, id: UUID) extends UniqueEntity

case class CourseProtocol(label: String, description: String, abbreviation: String, lecturer: UUID)

object Course extends UriGenerator[Course] with JsonSerialisation[CourseProtocol, Course] {

  override implicit def reads: Reads[CourseProtocol] = Json.reads[CourseProtocol]

  override implicit def writes: Writes[Course] = Json.writes[Course]

  override def base: String = "courses"
}
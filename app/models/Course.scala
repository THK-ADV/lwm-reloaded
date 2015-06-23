package models

import java.util.UUID
import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Course(label: String, lecturer: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Course extends UriGenerator[Course] with JsonSerialisation[Course] {
  def generateUri(course: Course)(implicit ns: Namespace): String = s"${ns}courses/${course.id}"

  override implicit def reads: Reads[Course] = Json.reads[Course]

  override implicit def writes: Writes[Course] = Json.writes[Course]
}
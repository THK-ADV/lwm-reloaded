package models

import java.util.UUID
import store.Namespace

case class Course(label: String, lecturer: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Course extends UriGenerator[Course] {
  def generateUri(course: Course)(implicit ns: Namespace): String = s"${ns}courses/${course.id}"
}
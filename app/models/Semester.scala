package models

import java.util.UUID

import store.Namespace

case class Semester(name: String, startDate: String, endDate: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Semester extends UriGenerator[Semester] {
  def generateUri(semester: Semester)(implicit ns: Namespace): String = s"${ns}semesters/${semester.id}"
}
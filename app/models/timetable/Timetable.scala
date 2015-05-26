package models.timetable

import java.util.UUID

import models._
import store.Namespace
import models.{UriGenerator, UniqueEntity}

case class Timetable(id: UUID = UUID.randomUUID()) extends UniqueEntity

//not included entries:[]

object Timetable extends UriGenerator[Timetable] {
  def generateUri(timetable: Timetable)(implicit ns: Namespace): String = s"${ns}timetables/${timetable.id}"
}
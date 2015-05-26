package models.timetable

import java.util.UUID

import models._
import store.Namespace

case class TimetableEntry(supervisor: String, room: String, startTime: String, endTime: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object TimetableEntry extends UriGenerator[TimetableEntry] {
  def generateUri(timetableEntry: TimetableEntry)(implicit ns: Namespace): String = s"${ns}timetableEntries/${timetableEntry.id}"
}

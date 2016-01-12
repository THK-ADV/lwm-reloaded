package models.schedule

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UriGenerator, UniqueEntity}
import org.joda.time.DateTime
import play.api.libs.json.{Format, Writes, Json, Reads}

case class Timetable(labwork: UUID, entries: Set[TimetableEntry], start: DateTime, blacklist: Set[DateTime], buffer: Int, id: UUID) extends UniqueEntity {

  // TODO: FIX THIS BLOODY DATETIME COMPARISON
  override def equals(that: scala.Any): Boolean = that match {
    case Timetable(l2, e2, s2, bl2, b2, id2) =>
      l2 == labwork && e2 == entries && s2.isEqual(start) && bl2.zip(blacklist).forall(a => a._1.isEqual(a._2)) && b2 == buffer && id2 == id
    case _ => false
  }
}

case class TimetableProtocol(labwork: UUID, entries: Set[TimetableEntry], start: DateTime, blacklist: Set[DateTime] = Set.empty[DateTime], buffer: Int = 0)

case class TimetableEntry(supervisor: UUID, room: UUID, degree: UUID, day: DateTime, start: DateTime, end: DateTime, date: DateTime, id: UUID = TimetableEntry.randomUUID) extends UniqueEntity {

  // TODO: FIX THIS BLOODY DATETIME COMPARISON
  override def equals(that: scala.Any): Boolean = that match {
    case TimetableEntry(sup2, room2, degree2, day2, start2, end2, date2, id2) =>
      supervisor == sup2 && room == room2 && degree2 == degree && day2.isEqual(day) && start2.isEqual(start) && end2.isEqual(end) && id2 == id && date2.isEqual(date)
    case _ => false
  }
}

case class TimetableEntryProtocol(supervisor: UUID, room: UUID, degree: UUID, day: DateTime, start: DateTime, end: DateTime, date: DateTime)

object Timetable extends UriGenerator[Timetable] with JsonSerialisation[TimetableProtocol, Timetable] {

  override def base: String = "timetables"

  override implicit def reads: Reads[TimetableProtocol] = Json.reads[TimetableProtocol]

  override implicit def writes: Writes[Timetable] = Json.writes[Timetable]
}

object TimetableEntry extends UriGenerator[TimetableEntry] with JsonSerialisation[TimetableEntryProtocol, TimetableEntry] {

  implicit def format: Format[TimetableEntry] = Json.format[TimetableEntry]

  override def base: String = "timetableEntries"

  override implicit def reads: Reads[TimetableEntryProtocol] = Json.reads[TimetableEntryProtocol]

  override implicit def writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]
}
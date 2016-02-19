package models.schedule

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.semester.Blacklist
import models.users.Employee
import models.{Room, UriGenerator, UniqueEntity}
import org.joda.time.{DateTime, LocalDateTime, LocalTime, LocalDate}
import play.api.libs.json.{Format, Writes, Json, Reads}
import services.ScheduleEntryG

case class Timetable(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Blacklist, id: UUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case Timetable(l2, e2, s2, bl2, id2) =>
      l2 == labwork && e2 == entries && s2.isEqual(start) && bl2 == localBlacklist && id2 == id
    case _ => false
  }
}

case class TimetableProtocol(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Blacklist = Blacklist.empty)

case class TimetableEntry(supervisor: UUID, room: UUID, degree: UUID, dayIndex: Int, start: LocalTime, end: LocalTime, id: UUID = TimetableEntry.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case TimetableEntry(sup2, room2, degree2, dayIndex2, start2, end2, id2) =>
      supervisor == sup2 && room == room2 && degree2 == degree && dayIndex2 == dayIndex && start2.isEqual(start) && end2.isEqual(end) && id2 == id
    case _ => false
  }
}

case class TimetableEntryProtocol(supervisor: UUID, room: UUID, degree: UUID, dayIndex: Int, start: LocalTime, end: LocalTime)

case class TimetableDateEntry(weekday: Weekday, date: LocalDate, start: LocalTime, end: LocalTime)

object Timetable extends UriGenerator[Timetable] with JsonSerialisation[TimetableProtocol, Timetable] {

  override def base: String = "timetables"

  override implicit def reads: Reads[TimetableProtocol] = Json.reads[TimetableProtocol]

  override implicit def writes: Writes[Timetable] = Json.writes[Timetable]
}

object TimetableEntry extends UriGenerator[TimetableEntry] with JsonSerialisation[TimetableEntryProtocol, TimetableEntry] {

  implicit val dateOrd = TimetableDateEntry.localDateOrd

  implicit val timeOrd = TimetableDateEntry.localTimeOrd

  implicit def format: Format[TimetableEntry] = Json.format[TimetableEntry]

  override def base: String = "timetableEntries"

  override implicit def reads: Reads[TimetableEntryProtocol] = Json.reads[TimetableEntryProtocol]

  override implicit def writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]
}

object TimetableDateEntry {

  case class Organizer(supervisor: UUID, room: UUID)

  def unravel(entries: Set[TimetableEntry], start: LocalDate): Set[TimetableDateEntry] = entries.map { entry =>
    val weekday = Weekday.toDay(entry.dayIndex)
    TimetableDateEntry(weekday, weekday.sync(start), entry.start, entry.end)
  }

  def organizer(entry: TimetableDateEntry, schema: Set[TimetableEntry]): Organizer = {
    schema.find(e => Weekday.toDay(e.dayIndex) == entry.weekday && e.start.isEqual(entry.start) && e.end.isEqual(entry.end)) match {
      case Some(s) => Organizer(s.supervisor, s.room)
      case None => Organizer(Employee.default.id, Room.default.id)
    }
  }

  def toLocalDateTime(entry: TimetableDateEntry): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }

  def toLocalDateTime(entry: ScheduleEntryG): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }

  def toDateTime(entry: TimetableDateEntry): DateTime = {
    entry.date.toDateTime(entry.start)
  }

  implicit val localTimeOrd: Ordering[LocalTime] = new Ordering[LocalTime] {
    override def compare(x: LocalTime, y: LocalTime): Int = x.compareTo(y)
  }

  implicit val localDateOrd: Ordering[LocalDate] = new Ordering[LocalDate] {
    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)
  }

  implicit val localDateTimeOrd: Ordering[LocalDateTime] = new Ordering[LocalDateTime] {
    override def compare(x: LocalDateTime, y: LocalDateTime): Int = x.compareTo(y)
  }
}
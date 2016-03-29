package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import models.semester.{Blacklist, BlacklistProtocol}
import models.users.Employee
import org.joda.time.{LocalDate, LocalDateTime, LocalTime}
import play.api.libs.json.{Format, Json, Reads, Writes}
import services.ScheduleEntryG

case class Timetable(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Blacklist, id: UUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case Timetable(l2, e2, s2, bl2, id2) =>
      l2 == labwork && e2 == entries && s2.isEqual(start) && bl2 == localBlacklist && id2 == id
    case _ => false
  }
}

case class TimetableProtocol(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: BlacklistProtocol)

case class TimetableAtom(labwork: Labwork, entries: Set[TimetableEntryAtom], start: LocalDate, localBlacklist: Blacklist, id: UUID)

case class TimetableEntry(supervisor: UUID, room: UUID, degree: UUID, dayIndex: Int, start: LocalTime, end: LocalTime) {

  override def equals(that: scala.Any): Boolean = that match {
    case TimetableEntry(sup2, room2, degree2, dayIndex2, start2, end2) =>
      supervisor == sup2 && room == room2 && degree2 == degree && dayIndex2 == dayIndex && start2.isEqual(start) && end2.isEqual(end)
    case _ => false
  }
}

case class TimetableEntryAtom(supervisor: Employee, room: Room, degree: Degree, dayIndex: Int, start: LocalTime, end: LocalTime)

case class TimetableDateEntry(weekday: Weekday, date: LocalDate, start: LocalTime, end: LocalTime)

object Timetable extends UriGenerator[Timetable] with JsonSerialisation[TimetableProtocol, Timetable] {

  import Blacklist.protocolFormat
  import TimetableEntry.atomicFormat

  override def base: String = "timetables"

  override implicit def reads: Reads[TimetableProtocol] = Json.reads[TimetableProtocol]

  override implicit def writes: Writes[Timetable] = Json.writes[Timetable]

  implicit def atomicWrites: Writes[TimetableAtom] = Json.writes[TimetableAtom]

  implicit def setAtomicWrites: Writes[Set[TimetableAtom]] = Writes.set[TimetableAtom](atomicWrites)
}

object TimetableEntry extends JsonSerialisation[TimetableEntry, TimetableEntry] {

  implicit val dateOrd = TimetableDateEntry.localDateOrd

  implicit val timeOrd = TimetableDateEntry.localTimeOrd

  implicit def format: Format[TimetableEntry] = Json.format[TimetableEntry]

  override implicit def reads: Reads[TimetableEntry] = Json.reads[TimetableEntry]

  override implicit def writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]

  implicit def atomicFormat: Format[TimetableEntryAtom] = Json.format[TimetableEntryAtom]

  implicit def atomicWrites: Writes[TimetableEntryAtom] = Json.writes[TimetableEntryAtom]

  implicit def setAtomicWrites: Writes[Set[TimetableEntryAtom]] = Writes.set[TimetableEntryAtom](atomicWrites)
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
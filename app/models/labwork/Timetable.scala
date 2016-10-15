package models.labwork

import java.util.UUID

import controllers.UserController
import controllers.crud.JsonSerialisation
import models._
import models.semester.Blacklist
import models.users.{Employee, User}
import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import play.api.libs.json._
import services.ScheduleEntryG

case class Timetable(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID = Timetable.randomUUID) extends UniqueEntity {

  import Blacklist.dateOrd

  override def equals(that: scala.Any): Boolean = that match {
    case Timetable(l2, e2, s2, bl2, _, id2) =>
      l2 == labwork &&
        e2 == entries &&
        s2.isEqual(start) &&
        bl2.toVector.sorted.zip(localBlacklist.toVector.sorted).forall(d => d._1.isEqual(d._2)) &&
        id2 == id
    case _ => false
  }
}

case class TimetableEntry(supervisor: Set[UUID], room: UUID, dayIndex: Int, start: LocalTime, end: LocalTime) {

  override def equals(that: scala.Any): Boolean = that match {
    case TimetableEntry(sup2, room2, dayIndex2, start2, end2) =>
      supervisor == sup2 && room == room2 && dayIndex2 == dayIndex && start2.isEqual(start) && end2.isEqual(end)
    case _ => false
  }
}

/**
  * Protocol
  */

case class TimetableProtocol(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Set[DateTime])

/**
  * Atom
  */

case class TimetableAtom(labwork: Labwork, entries: Set[TimetableEntryAtom], start: LocalDate, localBlacklist: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class TimetableEntryAtom(supervisor: Set[User], room: Room, dayIndex: Int, start: LocalTime, end: LocalTime)

/**
  * Helper
  */

case class TimetableDateEntry(weekday: Weekday, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, supervisor: Set[UUID])

object Timetable extends UriGenerator[Timetable] with JsonSerialisation[TimetableProtocol, Timetable, TimetableAtom] {

  import Blacklist.protocolFormat
  //import TimetableEntry.atomicFormat
  //import controllers.UserController.userFormat

  override def base: String = "timetables"

  override implicit def reads: Reads[TimetableProtocol] = Json.reads[TimetableProtocol]

  override implicit def writes: Writes[Timetable] = Json.writes[Timetable]

  override implicit def writesAtom: Writes[TimetableAtom] = new Writes[TimetableAtom] {
    override def writes(o: TimetableAtom): JsValue = {
      val a = Json.obj(
        "labwork" -> Labwork.writes.writes(o.labwork),
        "entries" -> JsArray(o.entries map TimetableEntry.writesAtom.writes toSeq),
        "start" -> o.start,
        "localBlacklist" -> JsArray(o.localBlacklist map (e => JsString(e.getMillis.toString)) toSeq)
      )

      o.invalidated.fold(a)(d => a + ("invalidated" -> JsString(d.toString()))) + ("id" -> JsString(o.id.toString))
    }
  }

  implicit def setAtomicWrites: Writes[Set[TimetableAtom]] = Writes.set[TimetableAtom]
}

object TimetableEntry extends JsonSerialisation[TimetableEntry, TimetableEntry, TimetableEntryAtom] {

  import controllers.UserController._

  implicit val dateOrd = TimetableDateEntry.localDateOrd

  implicit val timeOrd = TimetableDateEntry.localTimeOrd

  //implicit val fSet: Format[Set[User]] = setFormat[User](userFormat)

  implicit def format: Format[TimetableEntry] = Json.format[TimetableEntry]

  override implicit def reads: Reads[TimetableEntry] = Json.reads[TimetableEntry]

  override implicit def writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]

  override implicit def writesAtom: Writes[TimetableEntryAtom] = new Writes[TimetableEntryAtom] {
    override def writes(o: TimetableEntryAtom): JsValue = Json.obj(
      "supervisor" -> JsArray(o.supervisor map UserController.writes.writes toSeq),
      "room" -> Room.writes.writes(o.room),
      "dayIndex" -> o.dayIndex,
      "start" -> o.start,
      "end" -> o.end
    )
  }

  //implicit def atomicFormat: Format[TimetableEntryAtom] = Json.format[TimetableEntryAtom]

  implicit def setAtomicWrites: Writes[Set[TimetableEntryAtom]] = Writes.set[TimetableEntryAtom]
}

object TimetableDateEntry {

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
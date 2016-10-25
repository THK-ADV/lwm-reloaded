package models.labwork

import java.util.UUID

import controllers.UserController
import controllers.crud.JsonSerialisation
import models._
import models.users.User
import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import play.api.libs.json._
import services.ScheduleEntryG
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX

case class Timetable(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID = Timetable.randomUUID) extends UniqueEntity {

  import models.semester.Blacklist.dateOrd

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

  override def base: String = "timetables"

  override implicit def reads: Reads[TimetableProtocol] = Json.reads[TimetableProtocol]

  override implicit def writes: Writes[Timetable] = Json.writes[Timetable]

  override implicit def writesAtom: Writes[TimetableAtom] = TimetableAtom.writesAtom

  implicit def setAtomicWrites: Writes[Set[TimetableAtom]] = Writes.set[TimetableAtom]
}

object TimetableAtom{
  implicit def writesAtom: Writes[TimetableAtom] = (
    (JsPath \ "labwork").write[Labwork] and
      (JsPath \ "entries").writeSet[TimetableEntryAtom] and
      (JsPath \ "start").write[LocalDate] and
      (JsPath \ "localBlacklist").writeSet[DateTime] and
      (JsPath \ "invalidated").write[Option[DateTime]] and
      (JsPath \ "id").write[UUID]
    )(unlift(TimetableAtom.unapply))
}

object TimetableEntry extends JsonSerialisation[TimetableEntry, TimetableEntry, TimetableEntryAtom] {

  implicit val dateOrd = TimetableDateEntry.localDateOrd

  implicit val timeOrd = TimetableDateEntry.localTimeOrd

  override implicit def reads: Reads[TimetableEntry] = Json.reads[TimetableEntry]

  override implicit def writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]

  override implicit def writesAtom: Writes[TimetableEntryAtom] = TimetableEntryAtom.writesAtom

  implicit def setAtomicWrites: Writes[Set[TimetableEntryAtom]] = Writes.set[TimetableEntryAtom]
}

object TimetableEntryAtom{
  implicit def writesAtom: Writes[TimetableEntryAtom] = (
    (JsPath \ "supervisor").writeSet[User](UserController.writes) and
      (JsPath \ "room").write[Room](Room.writes)  and
      (JsPath \ "dayIndex").write[Int] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime]
    )(unlift(TimetableEntryAtom.unapply))
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
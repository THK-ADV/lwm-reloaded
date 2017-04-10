package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import services.ScheduleEntryG
import utils.Ops.JsPathX

case class Timetable(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID = Timetable.randomUUID) extends UniqueEntity {

  import models.LwmDateTime.dateTimeOrd

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

case class TimetableProtocol(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Set[String])

/**
  * Atom
  */

case class TimetableAtom(labwork: SesameLabwork, entries: Set[TimetableEntryAtom], start: LocalDate, localBlacklist: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class TimetableEntryAtom(supervisor: Set[User], room: SesameRoom, dayIndex: Int, start: LocalTime, end: LocalTime)

/**
  * Helper
  */

case class TimetableDateEntry(weekday: Weekday, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, supervisor: Set[UUID])

object Timetable extends UriGenerator[Timetable] with JsonSerialisation[TimetableProtocol, Timetable, TimetableAtom] {

  override def base: String = "timetables"

  override implicit def reads: Reads[TimetableProtocol] = Json.reads[TimetableProtocol]

  override implicit def writes: Writes[Timetable] = (
    (JsPath \ "labwork").write[UUID] and
      (JsPath \ "entries").writeSet[TimetableEntry] and
      (JsPath \ "start").write[LocalDate] and
      (JsPath \ "localBlacklist").writeSet[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(Timetable.unapply))

  override implicit def writesAtom: Writes[TimetableAtom] = TimetableAtom.writesAtom
}

object TimetableAtom {

  implicit def writesAtom: Writes[TimetableAtom] = (
    (JsPath \ "labwork").write[SesameLabwork] and
      (JsPath \ "entries").writeSet[TimetableEntryAtom] and
      (JsPath \ "start").write[LocalDate] and
      (JsPath \ "localBlacklist").writeSet[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(TimetableAtom.unapply))
}

object TimetableEntry extends JsonSerialisation[TimetableEntry, TimetableEntry, TimetableEntryAtom] {

  implicit val dateOrd = LwmDateTime.localDateOrd

  implicit val timeOrd = LwmDateTime.localTimeOrd

  override implicit def reads: Reads[TimetableEntry] = Json.reads[TimetableEntry]

  override implicit def writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]

  override implicit def writesAtom: Writes[TimetableEntryAtom] = TimetableEntryAtom.writesAtom
}

object TimetableEntryAtom {
  implicit def writesAtom: Writes[TimetableEntryAtom] = (
    (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "room").write[SesameRoom](SesameRoom.writes) and
      (JsPath \ "dayIndex").write[Int] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime]
    ) (unlift(TimetableEntryAtom.unapply))
}

object TimetableDateEntry {

  def toLocalDateTime(entry: TimetableDateEntry): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }

  def toLocalDateTime(entry: ScheduleEntryG): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }
}
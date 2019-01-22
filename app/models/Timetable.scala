package models

import java.util.UUID

import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime._
import utils.Ops.JsPathX

sealed trait TimetableLike extends UniqueEntity

case class Timetable(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Set[UUID], id: UUID = UUID.randomUUID) extends TimetableLike {
  override def equals(that: scala.Any) = that match {
    case Timetable(l, e, s, lb, i) =>
      l == labwork &&
        e == entries &&
        s.isEqual(start) &&
        lb == localBlacklist &&
        i == id
    case _ => false
  }
}

case class TimetableAtom(labwork: Labwork, entries: Set[TimetableEntryAtom], start: LocalDate, localBlacklist: Set[Blacklist], id: UUID = UUID.randomUUID) extends TimetableLike

case class TimetableProtocol(labwork: UUID, entries: Set[TimetableEntry], start: LocalDate, localBlacklist: Set[UUID])

case class TimetableEntry(supervisor: Set[UUID], room: UUID, dayIndex: Int, start: LocalTime, end: LocalTime) {
  override def equals(that: scala.Any) = that match {
    case TimetableEntry(s, r, d, st, et) =>
      s == supervisor &&
        r == room &&
        d == dayIndex &&
        st.isEqual(start) &&
        et.isEqual(end)
    case _ => false
  }
}

case class TimetableEntryAtom(supervisor: Set[User], room: Room, dayIndex: Int, start: LocalTime, end: LocalTime)

object Timetable {
  implicit val writes: Writes[Timetable] = Json.writes[Timetable]
}

object TimetableProtocol {
  implicit val reads: Reads[TimetableProtocol] = Json.reads[TimetableProtocol]
}

object TimetableEntry {
  implicit val reads: Reads[TimetableEntry] = Json.reads[TimetableEntry]

  implicit val writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]
}

object TimetableEntryAtom {

  implicit val writes: Writes[TimetableEntryAtom] = (
    (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "room").write[Room](Room.writes) and
      (JsPath \ "dayIndex").write[Int] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime]
    ) (unlift(TimetableEntryAtom.unapply))
}

object TimetableAtom {

  implicit val writes: Writes[TimetableAtom] = (
    (JsPath \ "labwork").write[Labwork](Labwork.writes) and
      (JsPath \ "entries").writeSet[TimetableEntryAtom] and
      (JsPath \ "start").write[LocalDate] and
      (JsPath \ "localBlacklist").writeSet[Blacklist](Blacklist.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(TimetableAtom.unapply))
}

object TimetableLike {

  implicit val writes: Writes[TimetableLike] = {
    case timetable: Timetable => Json.toJson(timetable)(Timetable.writes)
    case atom: TimetableAtom => Json.toJson(atom)(TimetableAtom.writes)
  }
}

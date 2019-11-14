package models

import java.util.UUID

import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX
import utils.date.DateTimeJsonFormatter._

sealed trait ScheduleEntryLike extends UniqueEntity {
  def labworkId: UUID
  def start: LocalTime
  def end: LocalTime
  def date: LocalDate
  def groupId: UUID
  def roomId: UUID
}

case class ScheduleEntry(labwork: UUID, start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: UUID, id: UUID = UUID.randomUUID) extends ScheduleEntryLike {
  override def labworkId = labwork

  override def groupId = group

  override def roomId = room
}

case class ScheduleEntryAtom(labwork: LabworkAtom, start: LocalTime, end: LocalTime, date: LocalDate, room: Room, supervisor: Set[User], group: Group, id: UUID) extends ScheduleEntryLike {
  override def labworkId = labwork.id

  override def groupId = group.id

  override def roomId = room.id
}

case class ScheduleEntryProtocol(labwork: UUID, start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: UUID)

object ScheduleEntry {
  implicit val writes: Writes[ScheduleEntry] = Json.writes[ScheduleEntry]
}

object ScheduleEntryProtocol {
  implicit val reads: Reads[ScheduleEntryProtocol] = Json.reads[ScheduleEntryProtocol]
}

object ScheduleEntryAtom {

  implicit val writes: Writes[ScheduleEntryAtom] = (
    (JsPath \ "labwork").write[LabworkAtom](LabworkAtom.writes) and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "room").write[Room](Room.writes) and
      (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "group").write[Group](Group.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(ScheduleEntryAtom.unapply))
}

object ScheduleEntryLike {

  implicit val writes: Writes[ScheduleEntryLike] = {
    case scheduleEntry: ScheduleEntry => Json.toJson(scheduleEntry)(ScheduleEntry.writes)
    case scheduleEntryAtom: ScheduleEntryAtom => Json.toJson(scheduleEntryAtom)(ScheduleEntryAtom.writes)
  }
}
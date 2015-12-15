package services

import models.Group
import models.schedule.{ScheduleEntry, Schedule, Timetable}

trait ScheduleServiceLike {

  def applyBlacklist(timetable: Timetable): Timetable

  def populate(amount: Int, timetable: Timetable, groups: Set[Group]): Vector[Schedule]
}

class ScheduleService extends ScheduleServiceLike {

  override def populate(amount: Int, timetable: Timetable, groups: Set[Group]): Vector[Schedule] = (0 until amount).map(_ => populate(timetable, groups)).toVector

  private def populate(timetable: Timetable, groups: Set[Group]): Schedule = {
    import scala.util.Random._

    val entries = timetable.entries.grouped(groups.size).flatMap(_.zip(shuffle(groups)).map {
      case (t, group) => ScheduleEntry(t.start, t.end, t.day, t.date, t.room, t.supervisor, group.id, ScheduleEntry.randomUUID)
    }).toSet

    Schedule(timetable.labwork, entries, Schedule.randomUUID)
  }

  override def applyBlacklist(timetable: Timetable): Timetable = timetable
}

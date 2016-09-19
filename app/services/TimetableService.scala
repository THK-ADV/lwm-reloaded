package services

import models.labwork._
import org.joda.time.{LocalDate, Weeks}

trait TimetableServiceLike {

  def extrapolateTimetableByWeeks(timetable: Timetable, weeks: Weeks, assignmentPlan: AssignmentPlan, groups: Set[Group]): Vector[TimetableDateEntry]
}

class TimetableService(private val blacklistService: BlacklistServiceLike) extends TimetableServiceLike {

  override def extrapolateTimetableByWeeks(timetable: Timetable, weeks: Weeks, assignmentPlan: AssignmentPlan, groups: Set[Group]): Vector[TimetableDateEntry] = {
    val appointments = assignmentPlan.entries.size * groups.size
    val schemaWeek = unravel(timetable.entries.toVector, timetable.start)

    val extrapolated = (0 until weeks.getWeeks).foldLeft(Vector.empty[TimetableDateEntry]) {
      case (vec, week) =>
        val nextWeek = schemaWeek.map(e => TimetableDateEntry(e.weekday, e.date.plusWeeks(week), e.start, e.end, e.room, e.supervisor))
        vec ++ nextWeek
    }

    val filtered = blacklistService.filterBy(extrapolated, timetable.localBlacklist)

    takeAppointments(filtered, assignmentPlan, groups.size) match {
      case enough if enough.size >= appointments => enough
      case _ => extrapolateTimetableByWeeks(timetable, weeks plus Weeks.ONE, assignmentPlan, groups)
    }
  }

  private def unravel(entries: Vector[TimetableEntry], start: LocalDate): Vector[TimetableDateEntry] = entries.map { entry =>
    val weekday = Weekday.toDay(entry.dayIndex)
    TimetableDateEntry(weekday, weekday.sync(start), entry.start, entry.end, entry.room, entry.supervisor)
  }

  private def takeAppointments(entries: Vector[TimetableDateEntry], assignmentPlan: AssignmentPlan, groupSize: Int): Vector[TimetableDateEntry] = {
    import TimetableDateEntry._

    val sorted = entries.sortBy(toLocalDateTime)
    val initial = sorted.take(groupSize)
    val remaining = sorted.drop(groupSize)

    assignmentPlan.entries.toVector.sortBy(_.index).drop(1).foldLeft(remaining, initial) {
      case ((e, vec), ae) =>
        val skip = groupSize * (ae.duration - 1)
        val remain = e.drop(skip)

        (remain.drop(groupSize), vec ++ remain.take(groupSize))
    }._2
  }
}

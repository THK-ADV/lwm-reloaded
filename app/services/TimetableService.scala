package services

import models.{Group, AssignmentPlan}
import models.schedule._

trait TimetableServiceLike {

  def extrapolateEntries(timetable: Timetable, assignmentPlan: AssignmentPlan, groups: Set[Group]): Set[TimetableDateEntry]
}

class TimetableService(private val blacklistService: BlacklistServiceLike) extends TimetableServiceLike {

  override def extrapolateEntries(timetable: Timetable, assignmentPlan: AssignmentPlan, groups: Set[Group]): Set[TimetableDateEntry] = {
    val w = 30 // TODO chose a proper value by calculating semester.end - semester.start in weeks

    extrapolateByWeeks(w, timetable, assignmentPlan, groups)
  }

  private def extrapolateByWeeks(weeks: Int, timetable: Timetable, assignmentPlan: AssignmentPlan, groups: Set[Group]): Set[TimetableDateEntry] = {
    val appointments = assignmentPlan.numberOfEntries * groups.size
    val schemaWeek = TimetableDateEntry.unravel(timetable.entries, timetable.start)

    val extrapolated = (0 until weeks).foldLeft((schemaWeek, Set.empty[TimetableDateEntry])) {
      case ((sw, set), week) =>
        val nextWeek = sw.map(e => TimetableDateEntry(e.weekday, e.date.plusWeeks(week), e.start, e.end))

        (sw, set ++ nextWeek)
    }._2

    val filtered = blacklistService.applyBlacklist(extrapolated, timetable.localBlacklist)

    takeAppointments(filtered, assignmentPlan, groups.size) match {
      case enough if enough.size >= appointments => enough
      case _ => extrapolateByWeeks(weeks * 2, timetable, assignmentPlan, groups)
    }
  }

  private def takeAppointments(entries: Set[TimetableDateEntry], assignmentPlan: AssignmentPlan, groupSize: Int): Set[TimetableDateEntry] = {
    import TimetableDateEntry._

    val sorted = entries.toVector.sortBy(toLocalDateTime)
    val initial = sorted.take(groupSize)
    val remaining = sorted.drop(groupSize)

    assignmentPlan.entries.toVector.sortBy(_.index).drop(1).foldLeft(remaining, initial) {
      case ((e, set), ae) =>
        val skip = groupSize * (ae.duration - 1)
        val remain = e.drop(skip)
        (remain.drop(groupSize), set ++ remain.take(groupSize))
    }._2.toSet
  }
}

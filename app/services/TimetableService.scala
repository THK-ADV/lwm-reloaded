package services

import models.{Group, AssignmentPlan}
import models.schedule.{TimetableEntry, TimetableProtocol, Timetable}

trait TimetableServiceLike {

  def extrapolateEntries(timetable: TimetableProtocol, assignmentPlan: AssignmentPlan, groups: Set[Group]): Timetable
}

class TimetableService(private val blacklistService: BlacklistServiceLike) extends TimetableServiceLike {

  override def extrapolateEntries(timetable: TimetableProtocol, assignmentPlan: AssignmentPlan, groups: Set[Group]): Timetable = {
    val w = 30 // TODO chose a proper value by calculating semester.end - semester.start in weeks

    extrapolateByWeeks(w, timetable, assignmentPlan, groups)
  }

  private def extrapolateByWeeks(weeks: Int, timetable: TimetableProtocol, assignmentPlan: AssignmentPlan, groups: Set[Group]): Timetable = {
    val appointments = assignmentPlan.numberOfEntries * groups.size

    val futureEntries = (0 until weeks).foldLeft((timetable, Set.empty[TimetableEntry])) {
      case ((t, vec), week) =>
        val future = t.entries.map {
          case e => TimetableEntry(e.supervisor, e.room, e.degree, e.day.plusWeeks(week), e.start.plusWeeks(week), e.end.plusWeeks(week), e.date.plusWeeks(week), e.id)
        }

        (t, vec ++ future)
    }._2

    val filtered = blacklistService.applyBlacklist(futureEntries, timetable.localBlacklist)
    takeAppointments(filtered, assignmentPlan, groups.size) match {
      case enough if enough.size >= appointments =>
        Timetable(timetable.labwork, enough, timetable.start, timetable.localBlacklist, Timetable.randomUUID)
      case _ =>
        extrapolateByWeeks(weeks * 2, timetable, assignmentPlan, groups)
    }
  }

  private def takeAppointments(entries: Set[TimetableEntry], assignmentPlan: AssignmentPlan, groupSize: Int): Set[TimetableEntry] = {
    import TimetableEntry.dateOrd

    val sorted = entries.toVector.sortBy(_.start)
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

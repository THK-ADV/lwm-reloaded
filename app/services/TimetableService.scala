package services

import models.{Group, AssignmentPlan}
import models.schedule.{TimetableEntry, TimetableProtocol, Timetable}
import org.joda.time.DateTime

trait TimetableServiceLike {

  def extrapolateEntries(timetable: TimetableProtocol, assignmentPlan: AssignmentPlan, groups: Set[Group]): Timetable

  def applyBlacklist(entries: Set[TimetableEntry], localBlacklist: Set[DateTime], globalBlacklist: Set[DateTime]): Set[TimetableEntry]
}

class TimetableService extends TimetableServiceLike {

  override def extrapolateEntries(timetable: TimetableProtocol, assignmentPlan: AssignmentPlan, groups: Set[Group]): Timetable = {
    val appointments = assignmentPlan.numberOfEntries * groups.size
    val w = 30 // TODO chose a proper value by calculating semester.end - semester.start in weeks

    extrapolateByWeeks(w, appointments, timetable)
  }

  private def extrapolateByWeeks(weeks: Int, appointments: Int, timetable: TimetableProtocol): Timetable = {
    import ScheduleG.dateOrd

    val futureEntries = (0 until weeks).foldLeft((timetable, Set.empty[TimetableEntry])) {
      case ((t, vec), week) => // TODO increase week counter depending on assignment plan buffers
        val future = t.entries.map {
          case e => TimetableEntry(e.supervisor, e.room, e.degree, e.day.plusWeeks(week), e.start.plusWeeks(week), e.end.plusWeeks(week), e.date.plusWeeks(week), e.id)
        }

        (t, vec ++ future)
    }._2

    applyBlacklist(futureEntries, timetable.blacklist, Set.empty[DateTime]) match {
      case enough if enough.size >= appointments =>
        val appEntries = enough.toVector.sortBy(_.start).take(appointments).toSet // toSet destroys ordering, but it works
        Timetable(timetable.labwork, appEntries.toSet, timetable.start, timetable.blacklist, timetable.buffer, Timetable.randomUUID)
      case _ =>
        extrapolateByWeeks(weeks * 2, appointments, timetable)
    }
  }

  override def applyBlacklist(entries: Set[TimetableEntry], localBlacklist: Set[DateTime], globalBlacklist: Set[DateTime]): Set[TimetableEntry] = {
    val blacklist = localBlacklist ++ globalBlacklist

    entries.map(e => (e, blacklist.find(_.isEqual(e.date)))).foldLeft(Set.empty[TimetableEntry]) {
      case (set, (_, Some(_))) => set
      case (set, (oe, None)) => set + oe
    }
  }
}

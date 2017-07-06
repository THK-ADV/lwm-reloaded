package services

import models._
import org.joda.time.{Interval, LocalDate, Weeks}

trait TimetableServiceLike {
  def extrapolateTimetableByWeeks(timetable: SesameTimetable, weeks: Weeks, assignmentPlan: SesameAssignmentPlan, groups: Set[SesameGroup]): Vector[TimetableDateEntry]
}

class TimetableService(private val blacklistService: BlacklistServiceLike) extends TimetableServiceLike {

  override def extrapolateTimetableByWeeks(timetable: SesameTimetable, weeks: Weeks, assignmentPlan: SesameAssignmentPlan, groups: Set[SesameGroup]): Vector[TimetableDateEntry] = {
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

  private def unravel(entries: Vector[SesameTimetableEntry], start: LocalDate): Vector[TimetableDateEntry] = entries.map { entry =>
    val weekday = Weekday.toDay(entry.dayIndex)
    TimetableDateEntry(weekday, weekday.sync(start), entry.start, entry.end, entry.room, entry.supervisor)
  }

  private def takeAppointments(entries: Vector[TimetableDateEntry], assignmentPlan: SesameAssignmentPlan, groupSize: Int): Vector[TimetableDateEntry] = {
    import models.TimetableDateEntry._
    import models.LwmDateTime.localDateTimeOrd

    val sorted = entries.sortBy(toLocalDateTime)
    val initial = sorted.take(groupSize)
    val remaining = sorted.drop(groupSize)

    assignmentPlan.entries.toVector.sortBy(_.index).drop(1).foldLeft((remaining, initial)) {
      case ((e, vec), ae) =>
        val skip = groupSize * (ae.duration - 1)
        val remain = e.drop(skip)

        (remain.drop(groupSize), vec ++ remain.take(groupSize))
    }._2
  }
}

object TimetableService {

  private def takeAppointments(entries: Vector[TimetableDateEntry], assignmentPlan: PostgresAssignmentPlan, groupSize: Int): Vector[TimetableDateEntry] = {
    import models.TimetableDateEntry._
    import models.LwmDateTime.localDateTimeOrd

    val sorted = entries.sortBy(toLocalDateTime)
    val initial = sorted.take(groupSize)
    val remaining = sorted.drop(groupSize)

    assignmentPlan.entries.toVector.sortBy(_.index).drop(1).foldLeft((remaining, initial)) {
      case ((e, vec), ae) =>
        val skip = groupSize * (ae.duration - 1)
        val remain = e.drop(skip)

        (remain.drop(groupSize), vec ++ remain.take(groupSize))
    }._2
  }

  def excludingBlacklists(entries: Vector[TimetableDateEntry], blacklists: Set[PostgresBlacklist]) = {
    entries.filterNot { e =>
      val entry = new Interval(e.date.toDateTime(e.start), e.date.toDateTime(e.end))

      blacklists.exists { b =>
        val blacklist = new Interval(b.date.toDateTime(b.start), b.date.toDateTime(b.end))

        entry overlaps blacklist
      }
    }
  }

  def extrapolateTimetableByWeeks(timetable: PostgresTimetableAtom,
                                  weeks: Weeks,
                                  assignmentPlan: PostgresAssignmentPlan,
                                  groupSize: Int): Vector[TimetableDateEntry] = {
    val appointments = assignmentPlan.entries.size * groupSize
    val schemaWeek = timetable.entries.toVector.map { entry =>
      val weekday = Weekday.toDay(entry.dayIndex)
      TimetableDateEntry(weekday, weekday.sync(timetable.start), entry.start, entry.end, entry.room.id, entry.supervisor.map(_.id))
    }

    val extrapolated = (0 until weeks.getWeeks).foldLeft(Vector.empty[TimetableDateEntry]) {
      case (vec, week) =>
        val nextWeek = schemaWeek.map(e => TimetableDateEntry(e.weekday, e.date.plusWeeks(week), e.start, e.end, e.room, e.supervisor))
        vec ++ nextWeek
    }

    val filtered = excludingBlacklists(extrapolated, timetable.localBlacklist)

    takeAppointments(filtered, assignmentPlan, groupSize) match {
      case enough if enough.size >= appointments => enough
      case _ => extrapolateTimetableByWeeks(timetable, weeks plus Weeks.ONE, assignmentPlan, groupSize)
    }
  }
}